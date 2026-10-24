;;; chirp-dm-inbox.el --- XChat inbox lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Appkit directory projection, pagination, and activation for XChat inboxes.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-chat-avatar)
(require 'appkit-directory)
(require 'appkit-bindings)
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
  (map-let (:name :handle) participant
    (or name (and handle (concat "@" handle)))))

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
  (map-let (:type :participants) conversation
    (when (eq type 'direct)
      (let ((self-id (chirp-dm-inbox--view-user-id view)))
        (or (cl-find-if
             (lambda (participant)
               (not (equal (map-elt participant :id) self-id)))
             participants)
            (car participants))))))

(defun chirp-dm-inbox--conversation-title (view conversation)
  "Return CONVERSATION's activity title as presented in VIEW."
  (pcase-let* (((map :title :type) conversation)
               (fallback (chirp-dm-inbox--one-line title)))
    (or (and (not (string-empty-p fallback)) fallback)
        (and-let* ((participant
                    (chirp-dm-inbox--participant-for-row view conversation)))
          (chirp-dm-inbox--participant-label participant))
        (if (eq type 'group)
            "Group conversation"
          "Direct message"))))

(defun chirp-dm-inbox--avatar-key (view conversation)
  "Return the avatar resource key representing CONVERSATION in VIEW."
  (when-let* ((participant
               (chirp-dm-inbox--participant-for-row view conversation)))
    (map-let (:id :avatar-url) participant
      (chirp-media-xchat-avatar-resource-key id avatar-url))))

(defun chirp-dm-inbox--state (view)
  "Return VIEW's validated XChat inbox state."
  (let ((state (appkit-surface-model view)))
    (unless (and (listp state)
                 (map-let (:type :instance) state
                   (and (eq type 'dm-inbox) instance)))
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
        :loading-p nil))

;;;; Projection

(defun chirp-dm-inbox--status-entry (state)
  "Return the passive status directory entry for inbox STATE, or nil."
  (pcase-let* (((map :phase :message) (chirp-dm-inbox--status state))
               (items (map-elt state :items))
               (text nil) (face nil))
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
  (map-let (:id :type :muted-p :message-request-p) conversation
  (appkit-directory-entry-create
   :key (list 'dm-conversation id)
   :role 'item
   :section-key '(dm-inbox recent)
   :label (chirp-dm-inbox--conversation-title view conversation)
   :primary-action 'item
   :item-p t
   :payload conversation
   :stamp (list (chirp-dm-inbox--preview-model view conversation)
                (chirp-dm-inbox--activity-time conversation)
                type muted-p message-request-p
                (chirp-dm-inbox--avatar-key view conversation))
   :help-echo "Open this conversation"
   :mouse-face 'highlight)))

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
  (map-let ((:inbox-preview preview) :latest-event) conversation
    (if (chirp-xchat-event-before-p
         latest-event (map-elt preview :latest-event))
        preview
      conversation)))

(defun chirp-dm-inbox--activity-time (conversation)
  "Return CONVERSATION's newest inbox activity timestamp."
  (plist-get (chirp-dm-inbox--activity conversation) :updated-at-msec))

(defun chirp-dm-inbox--preview-model (view conversation)
  "Return an activity-style one-line preview for CONVERSATION in VIEW."
  (pcase-let* ((activity (chirp-dm-inbox--activity conversation))
               ((map (:latest-event latest) (:preview raw-preview)) activity)
               ((map (:kind event-kind) :sender-id) latest)
         (sender
          (and sender-id
               (chirp-dm-inbox--participant conversation sender-id)))
         (self-id (chirp-dm-inbox--view-user-id view))
         (label
          (when (and latest
                     (eq event-kind 'message))
            (cond
             ((and self-id (equal sender-id self-id)) "You")
             ((eq (plist-get conversation :type) 'group)
              (or (chirp-dm-inbox--participant-label sender)
                  "Unknown sender")))))
         (preview (chirp-dm-inbox--one-line raw-preview)))
    (appkit-ui-one-line-preview-create
     :label label
     :separator (and label appkit-ui-sender-text-delimiter)
     :label-face (and label (or (appkit-name-color-face sender-id) 'bold))
     :text preview)))

(defun chirp-dm-inbox--context-trail (conversation)
  "Return status trail displayed beside CONVERSATION's inbox title."
  (map-let (:message-request-p :muted-p) conversation
    (string-join
     (delq nil
           (list
            (when message-request-p
              (propertize "request" 'face 'warning))
            (when muted-p
              (propertize "muted" 'face 'shadow))))
     " ")))

(defun chirp-dm-inbox--insert-avatar (view conversation)
  "Insert CONVERSATION's avatar or type fallback for inbox VIEW."
  (let* ((resource-key
          (and view (chirp-dm-inbox--avatar-key view conversation)))
         (image
          (and resource-key
               (chirp-media-avatar-resource-image view resource-key)))
         (fallback (if (eq (plist-get conversation :type) 'group) "#" "@"))
         (start (point)))
    (insert (appkit-chat-avatar-one-line-string image fallback))
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
     :icon-slot-width 'auto
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
                  (demand (map-let (:id :avatar-url) participant
                            (chirp-media-xchat-avatar-demand view id avatar-url))))
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

(defun chirp-dm-inbox--setup (_view)
  "Initialize the inbox's Appkit directory adapters."
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


(defun chirp-dm-inbox--effect (view request)
  "Describe REQUEST as one replaceable inbox Effect for VIEW."
  (appkit-command-start-effect
   (appkit-effect-create
    :key 'chirp-dm-inbox :lifetime 'request
    :input (list :app (appkit-surface-app view) :owner view :request request)
    :start
    (lambda (_context input _observe resolve reject)
      (let ((chirp--app (plist-get input :app)))
        (chirp-dm-state-request-cancellation
         (chirp-backend-dm-inbox
          (lambda (conversations envelope) (funcall resolve (list conversations envelope)))
          :cursor (plist-get (plist-get input :request) :cursor)
          :max-results chirp-dm-inbox-page-size
          :errback reject :owner (plist-get input :owner)))))
    :cancellation-requirement 'transport
    :success (lambda (input value)
               (list 'chirp-dm-inbox 'result (plist-get input :request) value nil))
    :failure (lambda (input error)
               (list 'chirp-dm-inbox 'result (plist-get input :request) nil error)))))

(defun chirp-dm-inbox--commit (context app input result)
  "Commit INPUT's scoped RESULT in APP and reply to the requesting Surface."
  (let* ((table (chirp--session-dm-conversations (appkit-app-model app)))
         (canonical (mapcar (lambda (snapshot)
                              (chirp-dm-state--acquire-preview table snapshot))
                            (car result))))
    (cons
     (appkit-command-post-message
      :target (appkit-transition-context-reply-route context) :delivery 'report
      :message (list 'chirp-dm-inbox 'committed input
                     (mapcar (lambda (item) (plist-get item :id)) canonical)
                     (chirp-backend-envelope-next-cursor (cadr result))))
     (mapcar (lambda (conversation)
               (chirp-dm-state--publish-command
                conversation (plist-get (car (plist-get conversation :events)) :id)))
             canonical))))

(defun chirp-dm-inbox--update (context model message)
  "Accept inbox MESSAGE through the owning Surface's update."
  (let* ((kind (nth 1 message))
         (view (appkit-current-surface))
         (status (chirp-dm-inbox--status model))
         (table (chirp--session-dm-conversations
                 (appkit-app-read-view-model
                  (appkit-transition-context-app-read-view context))))
         commands)
    (pcase kind
      ((or 'start 'canonical-resync)
       (let* ((phase (if (eq kind 'start) (nth 2 message) 'refresh))
              (cursor (and (eq phase 'older)
                           (plist-get (plist-get model :page) :next-cursor))))
         (setf (plist-get model :loading-p) t
               (plist-get status :phase) phase (plist-get status :message) nil)
         (setq commands
               (list (chirp-dm-inbox--effect
                      view (list :phase phase :cursor cursor))))))
      ('canonical-changed
       (let* ((payload (nth 2 message))
              (conversation (gethash (plist-get payload :id) table)))
         (when (and conversation (plist-get payload :promote))
           (setf (plist-get model :items)
                 (cons conversation (delq conversation
                                          (copy-sequence (plist-get model :items))))))))
      ('result
       (let ((input (nth 2 message)) (result (nth 3 message)) (error (nth 4 message)))
         (if error
             (setf (plist-get status :phase) 'error
                   (plist-get status :message) error
                   (plist-get model :loading-p) nil)
           (setq commands
                 (list (appkit-command-post-message
                        :target (appkit-transition-context-parent-address context)
                        :message (list 'chirp-dm-state 'inbox-commit (list input result))
                        :delivery 'report :reply-correlation 'chirp-dm-inbox))))))
      ('committed
       (let ((input (nth 2 message)))
         (let ((canonical (delq nil (mapcar (lambda (id) (gethash id table)) (nth 3 message))))
               (page (plist-get model :page)) (cursor (nth 4 message)))
           (setf (plist-get model :items)
                 (if (eq (plist-get input :phase) 'older)
                     (chirp-dm-inbox--append-unique
                      (plist-get model :items) canonical (lambda (item) (plist-get item :id)))
                   canonical)
                 (plist-get page :next-cursor) cursor
                 (plist-get page :exhausted-p) (not cursor)
                 (plist-get status :phase) 'idle (plist-get status :message) nil
                 (plist-get model :loading-p) nil))))
      ('failed
       (setf (plist-get status :phase) 'error
             (plist-get status :message) (nth 2 message)
             (plist-get model :loading-p) nil))
      (_ (error "Unsupported inbox message: %S" message)))
    (when (and (memq kind '(result committed failed)) (not (plist-get model :loading-p)))
      (push (appkit-command-cancel-effect 'chirp-dm-inbox) commands))
    (appkit-next :model model
                 :render (appkit-projection-change-create
                          :full-p t :frame-p t :position 'preserve)
                 :commands commands)))

(defun chirp-dm-inbox--request (view phase)
  "Ask VIEW's update to replace its inbox Effect with PHASE."
  (appkit-surface-send view (list 'chirp-dm-inbox 'start phase)))
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
  "N" #'chirp-dm-load-more-inbox)

(appkit-bindings-define 'chirp-dm-inbox--mode-map 'read-only
  'activate #'appkit-directory-activate
  'app-menu #'chirp-dispatch
  'refresh #'chirp-dm-refresh-inbox
  'next-item #'appkit-directory-next-item
  'previous-item #'appkit-directory-previous-item
  'next-button #'appkit-directory-tab-dwim
  'previous-button #'appkit-directory-previous-item
  'help #'describe-mode
  'quit #'chirp-quit-current-buffer
  'quit-cancel #'evil-quit)

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
