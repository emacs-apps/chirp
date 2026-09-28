;;; chirp-dm-inbox.el --- XChat inbox lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Appkit directory projection, pagination, and activation for XChat inboxes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-directory)
(require 'appkit-name-color)
(require 'appkit-surface)

(require 'appkit-ui)
(require 'appkit-presentation)
(require 'chirp-backend)
(require 'chirp-core)
(require 'chirp-media)
(require 'chirp-dm-state)
(require 'chirp-dm-conversation)
(require 'chirp-time)
(require 'chirp-x)

(defcustom chirp-dm-inbox-page-size 20
  "Number of XChat conversations requested per inbox page, up to 100."
  :type '(integer 1 100)
  :group 'chirp)

(defvar chirp-dm-inbox--next-instance 0
  "Monotonic identity source for fresh direct-message inbox views.")

(defconst chirp-dm-inbox--request-key 'dm-inbox
  "Operation key for one inbox view's active transport.")

;;; Implementation
(defconst chirp-dm-inbox--icon-slot-width 4
  "Columns reserved for one direct-message inbox avatar.")

;;; Inbox

(defun chirp-dm-inbox--one-line (text)
  "Return TEXT collapsed into one trimmed display line."
  (string-trim
   (replace-regexp-in-string "[[:space:]\n\r]+" " " (or text ""))))

(defun chirp-dm-inbox--format-time (milliseconds)
  "Return a compact local timestamp for MILLISECONDS, or an empty string."
  (if (and (stringp milliseconds)
           (string-match-p "\\`[0-9]+\\'" milliseconds))
      (chirp-time-format-compact
       (seconds-to-time (/ (string-to-number milliseconds) 1000)))
    ""))

(defun chirp-dm-inbox--status (state)
  "Return canonical status plist from direct-message STATE."
  (or (plist-get state :status)
      (error "Direct-message view state has no status")))

(defun chirp-dm-inbox--participant-label (participant)
  "Return PARTICIPANT's display label, or nil."
  (or (plist-get participant :name)
      (and-let* ((handle (plist-get participant :handle)))
        (concat "@" handle))))

(defun chirp-dm-inbox--participant (conversation sender-id)
  "Return SENDER-ID's participant from CONVERSATION."
  (cl-find sender-id (plist-get conversation :participants)
           :key (lambda (participant) (plist-get participant :id))
           :test #'equal))

(defun chirp-dm-inbox--view-user-id (view)
  "Return VIEW's current XChat user ID, or nil."
  (when (appkit-surface-live-p view)
    (chirp--session-xchat-user-id
     (appkit-app-model (appkit-surface-app view)))))

(defun chirp-dm-inbox--participant-for-row (view conversation)
  "Return the direct peer representing CONVERSATION in VIEW."
  (when (eq (plist-get conversation :type) 'direct)
    (let ((participants (plist-get conversation :participants))
          (self-id (chirp-dm-inbox--view-user-id view)))
      (or (cl-find-if
           (lambda (participant)
             (not (equal (plist-get participant :id) self-id)))
           participants)
          (car participants)))))

(defun chirp-dm-inbox--conversation-title (view conversation)
  "Return CONVERSATION's activity title as presented in VIEW."
  (let ((fallback (chirp-dm-inbox--one-line (plist-get conversation :title))))
    (or (and (not (string-empty-p fallback)) fallback)
        (and-let* ((participant
                    (chirp-dm-inbox--participant-for-row view conversation)))
          (chirp-dm-inbox--participant-label participant))
        (if (eq (plist-get conversation :type) 'group)
            "Group conversation"
          "Direct message"))))

(defun chirp-dm-inbox--avatar-key (view conversation)
  "Return the avatar resource key representing CONVERSATION in VIEW."
  (when-let* ((participant
               (chirp-dm-inbox--participant-for-row view conversation)))
    (chirp-media-xchat-avatar-resource-key
     (plist-get participant :id)
     (plist-get participant :avatar-url))))

(defun chirp-dm-inbox--state (view)
  "Return VIEW's validated XChat inbox state."
  (let ((state (appkit-surface-model view)))
    (unless
        (and (listp state) (eq (plist-get state :type) 'dm-inbox)
             (plist-get state :instance))
      (error "Invalid Chirp direct-message inbox state"))
    state))

(defun chirp-dm-inbox--current-view ()
  "Return the current live direct-message inbox view, or nil."
  (when-let*
      ((view (appkit-current-surface)) ((appkit-surface-live-p view))
       (state (appkit-surface-model view))
       ((eq (plist-get state :type) 'dm-inbox)))
    view))

(defun chirp-dm-inbox--make-state (instance)
  "Return canonical inbox state for INSTANCE."
  (list :type 'dm-inbox
        :instance instance
        :items nil
        :page (list :next-cursor nil :exhausted-p nil)
        :status (list :phase 'initial :message nil)
        :loading-p nil :request-token nil :request-handle nil))

;;;; Projection

(defun chirp-dm-inbox--status-entry (state)
  "Return the passive status directory entry for inbox STATE, or nil."
  (let* ((status (chirp-dm-inbox--status state))
         (phase (plist-get status :phase))
         (message (plist-get status :message))
         (items (plist-get state :items))
         text face)
    (pcase phase
      ('initial (setq text "Loading conversations…"))
      ('refresh (setq text "Refreshing conversations…"))
      ('older (setq text "Loading older conversations…"))
      ('error (setq text (format "Unable to load conversations: %s" message)
                    face 'error))
      (_ (when (null items)
           (setq text "No conversations returned."))))
    (when text
      (appkit-directory-entry-create
       :key '(dm-inbox status)
       :role 'note
       :label text
       :face face
       :stamp (list phase message (null items))))))

(defun chirp-dm-inbox--conversation-entry (view conversation)
  "Adapt normalized CONVERSATION to an Appkit directory entry for VIEW."
  (appkit-directory-entry-create
   :key (list 'dm-conversation (plist-get conversation :id))
   :role 'item
   :section-key '(dm-inbox recent)
   :label (chirp-dm-inbox--conversation-title view conversation)
   :primary-action 'item
   :item-p t
   :payload conversation
   :stamp (list (chirp-dm-inbox--preview-model view conversation)
                (chirp-dm-inbox--activity-time conversation)
                (plist-get conversation :type)
                (plist-get conversation :muted-p)
                (plist-get conversation :message-request-p)
                (chirp-dm-inbox--avatar-key view conversation))
   :help-echo "Open this conversation"
   :mouse-face 'highlight))

(defun chirp-dm-inbox--project (view state)
  "Project canonical inbox STATE into recent-session entries for VIEW."
  (let* ((items (plist-get state :items))
         (count (length items))
         (requests
          (cl-count-if
           (lambda (item) (plist-get item :message-request-p))
           items))
         (muted
          (cl-count-if
           (lambda (item) (plist-get item :muted-p))
           items)))
    (append
     (when items
       (list
        (appkit-directory-entry-create
         :key '(dm-inbox summary)
         :role 'note
         :label
         (format "%d conversation%s · %d request%s · %d muted"
                 count (if (= count 1) "" "s")
                 requests (if (= requests 1) "" "s")
                 muted)
         :face 'font-lock-doc-face
         :stamp (list count requests muted))
        (appkit-directory-entry-create
         :key '(dm-inbox recent)
         :role 'section
         :label "Recent Conversations"
         :face 'bold)))
     (mapcar
      (lambda (conversation)
        (chirp-dm-inbox--conversation-entry view conversation))
      items)
     (when-let* ((status-entry (chirp-dm-inbox--status-entry state)))
       (list status-entry)))))

(defun chirp-dm-inbox--activity (conversation)
  "Return the newest bounded preview or verified timeline activity."
  (let ((preview (plist-get conversation :inbox-preview)))
    (if (chirp-xchat-event-before-p
         (plist-get conversation :latest-event)
         (plist-get preview :latest-event))
        preview
      conversation)))

(defun chirp-dm-inbox--activity-time (conversation)
  "Return CONVERSATION's newest inbox activity timestamp."
  (plist-get (chirp-dm-inbox--activity conversation) :updated-at-msec))

(defun chirp-dm-inbox--preview-model (view conversation)
  "Return an activity-style one-line preview for CONVERSATION in VIEW."
  (let* ((activity (chirp-dm-inbox--activity conversation))
         (latest (plist-get activity :latest-event))
         (sender-id (and latest (plist-get latest :sender-id)))
         (sender
          (and sender-id
               (chirp-dm-inbox--participant conversation sender-id)))
         (self-id (chirp-dm-inbox--view-user-id view))
         (label
          (when (and latest
                     (eq (plist-get latest :kind) 'message))
            (cond
             ((and self-id (equal sender-id self-id)) "You")
             ((eq (plist-get conversation :type) 'group)
              (or (chirp-dm-inbox--participant-label sender)
                  "Unknown sender")))))
         (preview (chirp-dm-inbox--one-line (plist-get activity :preview))))
    (appkit-ui-one-line-preview-create
     :label label
     :separator (and label ":")
     :label-face (and label (or (appkit-name-color-face sender-id) 'bold))
     :text preview)))

(defun chirp-dm-inbox--context-trail (conversation)
  "Return status trail displayed beside CONVERSATION's inbox title."
  (string-join
   (delq nil
         (list
          (when (plist-get conversation :message-request-p)
            (propertize "request" 'face 'warning))
          (when (plist-get conversation :muted-p)
            (propertize "muted" 'face 'shadow))))
   " "))

(defun chirp-dm-inbox--insert-avatar (view conversation)
  "Insert CONVERSATION's avatar or type fallback for inbox VIEW."
  (let* ((resource-key
          (and view (chirp-dm-inbox--avatar-key view conversation)))
         (image
          (and resource-key
               (chirp-media-avatar-resource-image view resource-key)))
         (fallback (if (eq (plist-get conversation :type) 'group) "#" "@"))
         (start (point)))
    (if image
        (insert-image image fallback)
      (insert fallback))
    (add-text-properties
     start (point)
     (list 'face (if resource-key 'default 'shadow)
           'help-echo
           (if resource-key "Participant avatar"
             (if (eq (plist-get conversation :type) 'group)
                 "Group conversation"
               "Direct conversation"))))))

(defun chirp-dm-inbox--insert-item (_surface entry)
  "Insert one XChat inbox directory ENTRY."
  (let*
      ((conversation (appkit-directory-entry-payload entry))
       (view (appkit-current-surface)))
    (appkit-presentation-insert-one-line-row
     (appkit-presentation-one-line-row-create
      :icon-inserter
      (lambda ()
        (chirp-dm-inbox--insert-avatar
         view conversation))
      :context
      (appkit-directory-entry-label
       entry)
      :context-trail
      (chirp-dm-inbox--context-trail
       conversation)
      :preview
      (chirp-dm-inbox--preview-model
       view conversation)
      :time
      (chirp-dm-inbox--format-time
       (chirp-dm-inbox--activity-time conversation))
      :time-face 'shadow
      :time-tail-face nil
      :line-properties
      (list
       'chirp-dm-conversation-id
       (plist-get conversation
                  :id)
       'chirp-dm-message-request-p
       (and
        (plist-get
         conversation
         :message-request-p)
        t)
       'chirp-dm-muted-p
       (and
        (plist-get
         conversation :muted-p)
        t)))
     :indent 2
     :width (chirp--view-width)
     :icon-slot-width chirp-dm-inbox--icon-slot-width
     :context-width-spec '(0.34 18 36))))

(defun chirp-dm-inbox--activate-item (_surface entry)
  "Open the conversation carried by inbox directory ENTRY."
  (let ((conversation
         (copy-sequence (appkit-directory-entry-payload entry))))
    (setf (plist-get conversation :title)
          (or (appkit-directory-entry-label entry)
              (plist-get conversation :title)))
    (chirp-dm-conversation-open conversation :refresh-p t)))

(defun chirp-dm-inbox--sync (view _app state change)
  "Render committed inbox STATE using projection CHANGE."
  (let*
      ((directory (appkit-directory-surface))
       (resources (appkit-projection-change-resources change))
       (all-p
        (or (appkit-projection-change-geometry-p change)
            (memq 'all resources)))
       (keys
        (if all-p
            (hash-table-keys
             (appkit-directory-surface-node-table directory))
          (cl-loop for conversation in (plist-get state :items) when
                   (cl-member
                    (chirp-dm-inbox--avatar-key view conversation)
                    resources :test #'equal)
                   collect
                   (list 'dm-conversation (plist-get conversation :id))))))
    (when
        (or (appkit-projection-change-full-p change)
            (appkit-projection-change-geometry-p change)
            (appkit-projection-change-resources change)
            (appkit-projection-change-keys change))
      (appkit-directory-reconcile directory
                                  (chirp-dm-inbox--project view state)
                                  :force-keys keys)))
  (let (demands interests)
    (dolist (conversation (plist-get state :items))
      (when-let* ((participant (chirp-dm-inbox--participant-for-row view conversation))
                  (demand (chirp-media-xchat-avatar-demand
                           view (plist-get participant :id)
                           (plist-get participant :avatar-url))))
        (push demand demands)
        (let* ((key (appkit-resource-demand-key demand))
               (row-key (list 'dm-conversation (plist-get conversation :id)))
               (interest (cl-find key interests :key #'appkit-resource-interest-key
                                  :test #'equal)))
          (if interest
              (push row-key (appkit-resource-interest-row-keys interest))
            (push (appkit-resource-interest-create :key key :row-keys (list row-key))
                  interests)))))
    (appkit-render-result-create
     :resource-demands (cl-delete-duplicates demands :key #'appkit-resource-demand-key
                                             :test #'equal)
     :resource-interest-update
     (appkit-resource-interest-update-create :mode 'replace :entries interests))))

(defun chirp-dm-inbox--setup (view)
  "Initialize Appkit directory adapters for inbox VIEW." nil
  (setq-local chirp--view-title "Direct Messages")
  (setq-local header-line-format nil)
  (appkit-directory-configure (appkit-directory-surface)
                              :item-inserter #'chirp-dm-inbox--insert-item
                              :activate-function #'chirp-dm-inbox--activate-item
                              :action-rows-p t)
  nil nil)

;;;; Requests

(defun chirp-dm-inbox--append-unique (current fetched key-function)
  "Append unique FETCHED values to CURRENT using KEY-FUNCTION."
  (let ((seen (make-hash-table :test #'equal))
        additions)
    (dolist (item current)
      (puthash (funcall key-function item) t seen))
    (dolist (item fetched)
      (let ((key (funcall key-function item)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push item additions))))
    (append current (nreverse additions))))

(defun chirp-dm-inbox--request-current-p (view state token)
  "Return non-nil for the exact current inbox request TOKEN."
  (and (appkit-surface-live-p view)
       (eq state (appkit-surface-model view))
       (eq token (plist-get state :request-token))))

(defun chirp-dm-inbox--cancel-handle (handle)
  "Cancel one superseded inbox transport HANDLE."
  (cond
   ((and (appkit-handle-p handle) (appkit-handle-alive-p handle))
    (appkit-cancel-handle handle))
   ((buffer-live-p handle) (chirp-x-cancel-request handle))))

(defun chirp-dm-inbox--update (_context model message)
  "Accept inbox MESSAGE under its exact Surface-local MODEL."
  (if (not (eq model (nth 2 message)))
      (appkit-next-reject 'stale-inbox-state)
    (let ((kind (nth 1 message))
          (token (nth 3 message))
          (status (chirp-dm-inbox--status model)))
      (cond
       ((eq kind 'canonical-changed)
        (when (nth 4 message)
          (setf (plist-get model :items)
                (cons token (delq token
                                  (copy-sequence (plist-get model :items))))))
        (appkit-next
         :model model :render (appkit-projection-change-create
                               :full-p t :frame-p t :position 'preserve)))
       ((eq kind 'start)
        (setf (plist-get model :request-token) token
              (plist-get model :request-handle) nil
              (plist-get model :loading-p) t
              (plist-get status :phase) (nth 4 message)
              (plist-get status :message) nil)
        (appkit-next
         :model model :render (appkit-projection-change-create
                               :full-p t :frame-p t :position 'preserve)))
       ((not (eq token (plist-get model :request-token)))
        (appkit-next-reject 'stale-inbox-request))
       ((eq kind 'bind)
        (setf (plist-get model :request-handle) (nth 4 message))
        (appkit-next :model model :render appkit-render-none))
       ((memq kind '(success error))
        (if (eq kind 'success)
            (let ((canonical (nth 5 message))
                  (page (plist-get model :page))
                  (cursor (nth 6 message)))
              (setf (plist-get model :items)
                    (if (eq (nth 4 message) 'older)
                        (chirp-dm-inbox--append-unique
                         (plist-get model :items) canonical
                         (lambda (item) (plist-get item :id)))
                      canonical)
                    (plist-get page :next-cursor) cursor
                    (plist-get page :exhausted-p) (not cursor)
                    (plist-get status :phase) 'idle
                    (plist-get status :message) nil))
          (setf (plist-get status :phase) 'error
                (plist-get status :message) (nth 4 message)))
        (setf (plist-get model :loading-p) nil
              (plist-get model :request-token) nil
              (plist-get model :request-handle) nil)
        (appkit-next
         :model model :render (appkit-projection-change-create
                               :full-p t :frame-p t :position 'preserve)))
       (t (appkit-next-reject 'unknown-inbox-message))))))

(defun chirp-dm-inbox--request (view phase)
  "Start inbox request PHASE with exact latest-request ownership in VIEW."
  (let* ((state (chirp-dm-inbox--state view))
         (app (appkit-surface-app view))
         (token (list chirp-dm-inbox--request-key))
         (previous (plist-get state :request-handle))
         (cursor (and (eq phase 'older)
                      (plist-get (plist-get state :page) :next-cursor))))
    ;; Revoke the old token before cancellation can invoke its errback.
    (appkit-surface-send view (list 'chirp-dm-inbox 'start state token phase))
    (chirp-dm-inbox--cancel-handle previous)
    (let* ((chirp--app app)
           (handle
            (chirp-backend-dm-inbox
             (lambda (conversations envelope)
               (when (chirp-dm-inbox--request-current-p view state token)
                 (let* ((chirp--app app)
                        (canonical
                         (mapcar #'chirp-dm-state-acquire-preview conversations)))
                   (appkit-surface-send
                    view (list 'chirp-dm-inbox 'success state token phase
                               canonical
                               (chirp-backend-envelope-next-cursor envelope))))))
             :cursor cursor :max-results chirp-dm-inbox-page-size
             :errback
             (lambda (text)
               (when (chirp-dm-inbox--request-current-p view state token)
                 (appkit-surface-send
                  view (list 'chirp-dm-inbox 'error state token text))))
             :owner view)))
      (if (chirp-dm-inbox--request-current-p view state token)
          (appkit-surface-send
           view (list 'chirp-dm-inbox 'bind state token handle))
        (chirp-dm-inbox--cancel-handle handle)))))

(defun chirp-dm-inbox-refresh-live-view (view)
  "Start a fallback live refresh for inbox VIEW when it is idle.\n\nReturn non-nil when the refresh was accepted."
  (when
      (and (appkit-surface-live-p view)
           (eq (plist-get (appkit-surface-model view) :type) 'dm-inbox)
           (not (plist-get (appkit-surface-model view) :loading-p)))
    (chirp-dm-inbox--request view 'refresh) t))

(defun chirp-dm-refresh-inbox ()
  "Refresh the current XChat inbox without sending a read acknowledgment."
  (interactive)
  (if-let* ((view (chirp-dm-inbox--current-view)))
      (chirp-dm-inbox--request view 'refresh)
    (user-error "Current view is not a direct-message inbox")))

(defun chirp-dm-load-more-inbox ()
  "Load one older page in the current XChat inbox."
  (interactive)
  (if-let* ((view (chirp-dm-inbox--current-view)))
      (let* ((state (chirp-dm-inbox--state view))
             (page (plist-get state :page)))
        (cond
         ((plist-get state :loading-p)
          (user-error "A direct-message inbox request is already running"))
         ((plist-get page :exhausted-p)
          (user-error "No older conversations available"))
         ((null (plist-get page :next-cursor))
          (user-error "Direct-message inbox cursor is unavailable"))
         (t
          (chirp-dm-inbox--request view 'older))))
    (user-error "Current view is not a direct-message inbox")))

;;; Inbox Mode

(defvar-keymap chirp-dm-inbox--mode-map
  :doc "Keymap for `chirp-dm-inbox--mode'."
  :parent appkit-directory-mode-map
  "g" #'chirp-dm-refresh-inbox
  "N" #'chirp-dm-load-more-inbox
  "q" #'chirp-quit-current-buffer)

(define-derived-mode chirp-dm-inbox--mode appkit-directory-mode "Chirp-DMs"
  "Major mode for Chirp's Appkit-owned XChat inbox."
  (setq-local revert-buffer-function
              (lambda (&rest _ignored)
                (chirp-dm-refresh-inbox))))

(defun chirp-dm-inbox-open ()
  "Open an unlocked read-only XChat inbox and return its buffer."
  (let*
      ((instance (cl-incf chirp-dm-inbox--next-instance))
       (view
        (chirp-open-projection-view
         :id (list 'dm-inbox instance)
         :mode 'chirp-dm-inbox--mode
         :title "Direct Messages"
         :state
         (chirp-dm-inbox--make-state
          instance)
         :render-function #'chirp-dm-inbox--sync
         :anchor-property appkit-directory-key-property
         :setup #'chirp-dm-inbox--setup
         :select t)))
    (chirp-dm-inbox--request view 'initial)
    (appkit-surface-buffer view)))

(provide 'chirp-dm-inbox)

;;; chirp-dm-inbox.el ends here
