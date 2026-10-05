;;; chirp-timeline.el --- Timeline views for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The primary Surface reducer owns pagination in session-retained feed plists.
;; Managed Effects own transport and post-commit asset preparation; callbacks
;; only settle their Effect.  Collections below retain their separate protocol.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'appkit-core)
(require 'appkit-command)
(require 'appkit-effect)
(require 'appkit-projection)
(require 'appkit-surface)

(require 'appkit-position)
(require 'appkit-presentation)
(require 'appkit-scroll)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)
(require 'chirp-x)

(declare-function chirp-profile-load-more "chirp-profile" (&optional anchor-id))

;;; Primary Timeline
;;;; Options

(defcustom chirp-timeline-poll-interval 30
  "Seconds between background checks for new primary-timeline posts.

Set this to nil to disable automatic checks.  Manual refreshes with `g' still
check for new posts without immediately moving the current timeline."
  :type '(choice (const :tag "Disable automatic checks" nil)
                 number)
  :group 'chirp)

(defcustom chirp-timeline-auto-load-threshold 2000
  "Character distance from the visible timeline end that loads older posts.

Set this to nil to disable automatic pagination.  Manual loading with
`chirp-load-more' remains available."
  :type '(choice (const :tag "Disable automatic pagination" nil)
                 integer)
  :group 'chirp)

(defun chirp-timeline--title (kind)
  "Return the buffer title for timeline KIND."
  (pcase kind
    ('home "For You")
    ('following "Following")
    (_ "Timeline")))

;;;; Constants

(defconst chirp-timeline--primary-view-id 'primary-timeline
  "Stable Appkit view identity shared by Home and Following.")

(defconst chirp-timeline--request-key 'primary-timeline
  "Operation key for the active primary timeline transport.")

(cl-defstruct (chirp-timeline--generation
               (:constructor chirp-timeline--generation-create))
  "One logical primary timeline request generation."
  phase
  quiet-p)

;;;; State and Mode

(define-derived-mode chirp-timeline--mode chirp-view-mode "Chirp-Timeline"
  "Major mode for Appkit-owned primary timeline buffers."
  (setq-local chirp--refresh-function #'chirp-timeline--refresh-primary)
  (setq-local chirp--entry-wrap-navigation nil)
  (setq-local chirp-timeline--scroll-observer nil)
  (setq-local header-line-format nil))

(keymap-set chirp-timeline--mode-map "." #'chirp-timeline-show-new)

(defvar chirp-timeline--header-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-1] #'ignore)
    (define-key map [header-line mouse-1] #'chirp-timeline-show-new)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Mouse keymap for the pending-new-posts header-line button.")

(defvar-local chirp-timeline--scroll-observer nil
  "Lifecycle-owned visible-end observer for the primary timeline.")

(defun chirp-timeline--make-state (kind limit)
  "Return canonical state for primary timeline KIND and LIMIT."
  (list :type 'timeline
        :query (list :kind kind :limit limit)
        :items nil
        :pending-new-items nil
        :page (list :next-cursor nil
                    :exhausted-p nil
                    :auto-load-paused-p nil)
        :status (list :phase 'initial :message nil)
        :generation nil
        :loaded-p nil
        :position nil
        :media-intent nil
        :media-phase 'idle
        :media-error nil
        :expanded-tweet-ids (make-hash-table :test #'equal)))

(defun chirp-timeline--feed-state (kind)
  "Return the session-retained primary feed state for KIND.
Keep this plist's identity: the active Surface reducer writes pagination and
positions, while the App reducer updates retained tweet and quote contents."
  (unless (memq kind '(home following))
    (error "Invalid Chirp primary feed kind: %S" kind))
  (let ((states
         (chirp--session-primary-feed-states (chirp--session))))
    (or (gethash kind states)
        (puthash kind
                 (chirp-timeline--make-state
                  kind chirp-default-max-results)
                 states))))

(defun chirp-timeline--view-state (view)
  "Return VIEW's validated primary timeline state."
  (let ((state (appkit-surface-model view)))
    (unless
        (and (listp state) (eq (plist-get state :type) 'timeline)
             (memq (plist-get (plist-get state :query) :kind)
                   '(home following)))
      (error "Invalid Chirp timeline view state"))
    state))

(defun chirp-timeline--list-state (view)
  "Return VIEW's validated tweet-list projection state."
  (let ((state (appkit-surface-model view)))
    (unless
        (and (listp state)
             (memq (plist-get state :type) '(timeline collection))
             (plist-get (plist-get state :query) :kind))
      (error "Invalid Chirp tweet-list view state"))
    state))

(defun chirp-timeline--current-view ()
  "Return the current live primary timeline view, or nil."
  (when-let*
      ((view (appkit-current-surface)) ((appkit-surface-live-p view))
       (state (appkit-surface-model view))
       ((eq (plist-get state :type) 'timeline))
       ((memq (plist-get (plist-get state :query) :kind)
              '(home following))))
    view))

;;;; Projection

(defun chirp-timeline--frame-text (state)
  "Return generated frame text representing timeline STATE."
  (pcase-let* (((map :status :title :items) state)
               ((map :phase :message) status)
               (title (downcase (or title "timeline"))))
    (pcase phase
      ('initial (format "Loading %s...\n\n" title))
      ('refresh "Checking for new posts...\n\n")
      ('older "Loading older posts...\n\n")
      ('error (format "Unable to load data.\n\n%s\n\n" message))
      (_ (and (null items) "No posts returned.\n")))))

(defun chirp-timeline--header-line ()
  "Return the pending-new-posts header line for the current timeline."
  (when-let* ((view (chirp-timeline--current-view))
              (state (chirp-timeline--view-state view))
              (pending (plist-get state :pending-new-items)))
    (let* ((count (length pending))
           (label (format "Show %d post%s" count (if (= count 1) "" "s")))
           (window (get-buffer-window (current-buffer) 'visible))
           (width (if window (window-body-width window) fill-column))
           (padding (max 0 (/ (- width (string-width label)) 2))))
      (concat
       (make-string padding ?\s)
       (propertize
        label
        'face 'chirp-link-face
        'keymap chirp-timeline--header-line-map
        'mouse-face 'mode-line-highlight
        'help-echo "Mouse-1: Show pending posts"
        'follow-link 'ignore)))))

(defun chirp-timeline--sync-header-line (view state)
  "Synchronize VIEW's conditional header line from timeline STATE."
  (with-current-buffer (appkit-surface-buffer view)
    (let
        ((format
          (and (plist-get state :pending-new-items)
               '(:eval (chirp-timeline--header-line)))))
      (unless (equal header-line-format format)
        (setq-local header-line-format format)
        (force-mode-line-update)))))

(defun chirp-timeline--remember-position ()
  "Commit durable feed positions before the primary buffer is detached."
  (when-let* ((view (chirp-timeline--current-view)))
    (let ((state (chirp-timeline--view-state view)))
      (chirp-timeline--dispatch
       view 'detach
       (and (plist-get state :items)
            (appkit-position-capture
             :anchor-property 'chirp-entry-id :preserve-window-start t))
       (chirp--primary-feed-state-values state)))))

(defun chirp-timeline--maybe-auto-load-older
    (view _window position end)
  "Quietly load older posts for VIEW when POSITION approaches END."
  (when
      (and (appkit-surface-live-p view)
           (numberp chirp-timeline-auto-load-threshold)
           (appkit-scroll-near-end-p position end
                                     chirp-timeline-auto-load-threshold))
    (pcase-let*
        ((state (chirp-timeline--view-state view))
         ((map :page :status :loaded-p :generation) state)
         ((map :phase) status)
         ((map :next-cursor :auto-load-paused-p :exhausted-p) page))
      (when
          (and loaded-p (eq phase 'idle) (null generation)
               next-cursor (not auto-load-paused-p) (not exhausted-p))
        (chirp-timeline--load-more-primary view t)))))

(defun chirp-timeline--install-scroll-observer (view)
  "Install VIEW's lifecycle-owned older-page observer."
  (setq-local
   chirp-timeline--scroll-observer
   (appkit-scroll-observer-install
    view
    :end-function
    (lambda (window position end)
      (chirp-timeline--maybe-auto-load-older view window position end)))))

(defun chirp-timeline--setup-view (view)
  "Mount the primary timeline presentation cache."
  (let* ((state (chirp-timeline--view-state view))
         (kind (plist-get (plist-get state :query) :kind)))
    (chirp--setup-projection-view view (chirp-timeline--title kind)
                                  #'chirp-render-print-tweet-row 'chirp-entry-id)
    (setq-local chirp--refresh-function #'chirp-timeline--refresh-primary)
    (add-hook 'kill-buffer-hook #'chirp-timeline--remember-position nil t)))

(defun chirp-timeline--sync (surface _app state change)
  "Render committed timeline STATE using native projection CHANGE."
  (when (eq (plist-get state :type) 'timeline)
    (let ((title (chirp-timeline--title
                  (plist-get (plist-get state :query) :kind))))
      (unless (equal chirp--view-title title)
        (setq-local chirp--view-title title)
        (chirp--apply-buffer-name (appkit-surface-buffer surface) title))))
  (chirp-timeline--sync-header-line surface state)
  (chirp-render-projection
      surface change
    (chirp-render-project-tweet-rows (plist-get state :items))
    (chirp-timeline--frame-text state))
  nil)

;;;; Requests

(defun chirp-timeline--fetch-count (state phase)
  "Return request size for STATE and request PHASE."
  (let ((limit (plist-get (plist-get state :query) :limit)))
    (pcase phase
      ('older (max 1 chirp-timeline-load-more-step))
      ((or 'refresh 'poll)
       (min limit
            (or (and chirp-timeline-refresh-max-results
                     (max 1 chirp-timeline-refresh-max-results))
                limit)))
      (_ limit))))

(defun chirp-timeline--dispatch (view action &rest data)
  "Admit data-only ACTION and DATA for VIEW's exact current model."
  (appkit-surface-send
   view (append (list 'chirp-timeline action
                      (chirp-timeline--view-state view) view)
                data)))

(defun chirp-timeline--next (state &optional position commands)
  "Project committed STATE at POSITION and emit closed COMMANDS."
  (appkit-next
   :model state
   :render (appkit-projection-change-create
            :full-p t :frame-p t :position (or position 'preserve))
   :commands commands))

(defun chirp-timeline--fetch-start (_context input _observe resolve reject)
  "Start INPUT's feed transport; callbacks may only settle this Effect."
  (pcase-let* (((map :owner :kind :count :cursor) input)
               (chirp--app (appkit-surface-app owner))
               (request (chirp-backend-feed
                         resolve (eq kind 'following) reject count cursor owner)))
    (appkit-cancellation-create
     :kind 'transport
     :cancel (lambda ()
               (when (or (appkit-handle-p request) (buffer-live-p request))
                 (chirp-x-cancel-request request))))))

(defun chirp-timeline--fetched (input tweets envelope)
  "Map INPUT's terminal payload to a data-only feed result."
  (list 'chirp-timeline 'success
        (plist-get input :state) (plist-get input :owner)
        (plist-get input :generation) tweets
        (chirp-backend-envelope-next-cursor envelope)))

(defun chirp-timeline--fetch-failed (input text)
  "Map INPUT's failure TEXT to a data-only feed result."
  (list 'chirp-timeline 'error
        (plist-get input :state) (plist-get input :owner)
        (plist-get input :generation) text))

(defun chirp-timeline--begin-request (view state phase quiet)
  "Commit PHASE in STATE and return its managed request command for VIEW.
Only the Surface reducer calls this function.  The Effect runtime owns the
transport handle, including synchronous completion and revoke-before-cancel."
  (let* ((generation
          (chirp-timeline--generation-create :phase phase :quiet-p quiet))
         (query (plist-get state :query))
         (status (plist-get state :status)))
    (setf (plist-get state :generation) generation
          (plist-get status :phase) phase
          (plist-get status :message) nil)
    (chirp-timeline--next
     state nil
     (list
      (appkit-command-start-effect
       (appkit-effect-create
        :key chirp-timeline--request-key
        :input (list :owner view :state state :generation generation
                     :kind (plist-get query :kind)
                     :count (chirp-timeline--fetch-count state phase)
                     :cursor (and (eq phase 'older)
                                  (plist-get (plist-get state :page) :next-cursor)))
        :start #'chirp-timeline--fetch-start
        :success #'chirp-timeline--fetched
        :failure #'chirp-timeline--fetch-failed
        :cancellation-requirement 'transport))))))

(defun chirp-timeline--followup-done (input &rest _result)
  "Map completed finite follow-up INPUT back to its exact model."
  (list 'chirp-timeline 'prepared
        (plist-get input :state) (plist-get input :owner)))

(defun chirp-timeline--assets-failed (input text)
  "Report asset preparation failure TEXT without changing feed status."
  (list 'chirp-timeline 'assets-failed
        (plist-get input :state) (plist-get input :owner) text))

(defun chirp-timeline--assets-start (_context input _observe resolve reject)
  "Prepare committed feed assets outside Appkit's active-pass gate.
The existing quote/cache APIs can complete synchronously and publicly admit
messages.  This finite, cancelable timer is only their fixed-purpose bridge;
it neither accepts a thunk nor changes polling or shared media ownership."
  (let ((active t) timer)
    (setq timer
          (run-at-time
           0 nil
           (lambda ()
             (when active
               (setq active nil)
               (condition-case condition
                   (pcase-let (((map :owner :state) input))
                     (when (and (appkit-surface-live-p owner)
                                (eq state (appkit-surface-model owner)))
                       (let ((chirp--app (appkit-surface-app owner))
                             (buffer (appkit-surface-buffer owner))
                             ;; Include all committed rows so folded follow-ups
                             ;; cannot lose an earlier page's preparation.
                             (tweets (append (plist-get state :items)
                                             (plist-get state :pending-new-items))))
                         (chirp-media-prefetch-tweets tweets buffer)
                         (chirp-enrich-quoted-tweets tweets buffer)))
                     (funcall resolve))
                 ((error quit)
                  (funcall reject (error-message-string condition))))))))
    (appkit-cancellation-create
     :kind 'logical
     :cancel (lambda () (setq active nil) (cancel-timer timer)))))

(defun chirp-timeline--assets-command (view state)
  "Return the bounded asset-preparation command for committed STATE in VIEW."
  (appkit-command-start-effect
   (appkit-effect-create
    :key 'primary-timeline-assets
    :input (list :owner view :state state)
    :start #'chirp-timeline--assets-start
    :success #'chirp-timeline--followup-done
    :failure #'chirp-timeline--assets-failed)))

(defun chirp-timeline--feedback-start (_context input _observe resolve _reject)
  "Present committed feedback and schedule the existing visible-end observer."
  (pcase-let (((map :owner :notice :recheck) input))
    (when notice (message "%s" notice))
    (when recheck
      (with-current-buffer (appkit-surface-buffer owner)
        (when (appkit-scroll-observer-p chirp-timeline--scroll-observer)
          (appkit-scroll-observer-check chirp-timeline--scroll-observer))))
    (funcall resolve))
  nil)

(defun chirp-timeline--feedback-command (view state notice &optional recheck)
  "Return one finite feedback command for NOTICE and optional RECHECK."
  (appkit-command-start-effect
   (appkit-effect-create
    :key 'primary-timeline-feedback
    :input (list :owner view :state state :notice notice :recheck recheck)
    :start #'chirp-timeline--feedback-start
    :success #'chirp-timeline--followup-done
    :failure #'chirp-timeline--assets-failed)))

(defun chirp-timeline--reset-request (state)
  "Revoke STATE's logical generation in the Surface reducer.
Physical cancellation belongs exclusively to the returned Effect commands."
  (when (plist-get state :generation)
    (let ((status (plist-get state :status)))
      (setf (plist-get state :generation) nil
            (plist-get status :phase) (if (plist-get state :loaded-p) 'idle 'initial)
            (plist-get status :message) nil))))

(defun chirp-timeline--cancel-commands ()
  "Return the closed cancellation commands for primary feed work."
  (mapcar #'appkit-command-cancel-effect
          (list chirp-timeline--request-key
                'primary-timeline-assets 'primary-timeline-feedback)))

(defun chirp-timeline--success-update (view state generation tweets next-cursor)
  "Commit an authorized page to STATE; all I/O is returned as Effects."
  (pcase-let*
      ((phase (chirp-timeline--generation-phase generation))
       (quiet (chirp-timeline--generation-quiet-p generation))
       ((map :query :page :status (:items current)) state)
       (position 'preserve) (notice nil))
    (pcase phase
      ('older
       (let* ((cursor (plist-get page :next-cursor))
              (merged (chirp-append-unique-tweets current tweets)))
         (unless (or (> (length merged) (length current)) quiet)
           (setq notice "No older posts."))
         (setf (plist-get state :items) merged
               (plist-get page :next-cursor) next-cursor
               (plist-get page :exhausted-p)
               (or (not next-cursor) (equal cursor next-cursor))
               (plist-get page :auto-load-paused-p) nil)))
      ((or 'refresh 'poll)
       (if (null current)
           (setf (plist-get state :items) tweets
                 (plist-get state :pending-new-items) nil
                 (plist-get page :next-cursor) next-cursor
                 (plist-get page :exhausted-p) (not next-cursor)
                 position 'first)
         (let ((pending (chirp-timeline--stage-new-tweets
                         current (plist-get state :pending-new-items) tweets)))
           (when (eq phase 'refresh)
             (setq notice (chirp-timeline--refresh-message (length pending))))
           (setf (plist-get state :pending-new-items) pending
                 (plist-get page :next-cursor)
                 (if (> (length current) (plist-get query :limit))
                     (plist-get page :next-cursor)
                   (or next-cursor (plist-get page :next-cursor)))))))
      (_
       (setf (plist-get state :items) tweets
             (plist-get state :pending-new-items) nil
             (plist-get page :next-cursor) next-cursor
             (plist-get page :exhausted-p) (not next-cursor)
             (plist-get page :auto-load-paused-p) nil)
       (setq position 'first)))
    (setf (plist-get status :phase) 'idle
          (plist-get status :message) nil
          (plist-get state :generation) nil
          (plist-get state :loaded-p) t)
    (chirp-timeline--next
     state position
     (append
      (list (chirp-timeline--assets-command view state))
      (when (or notice (and quiet (eq phase 'older)))
        (list (chirp-timeline--feedback-command
               view state notice (and quiet (eq phase 'older)))))))))

(defun chirp-timeline--error-update (view state generation text)
  "Commit an authorized request failure TEXT without disturbing quiet reads."
  (let* ((phase (chirp-timeline--generation-phase generation))
         (quiet (chirp-timeline--generation-quiet-p generation))
         (silent (or quiet (eq phase 'poll)))
         (status (plist-get state :status)))
    (setf (plist-get status :phase) (if silent 'idle 'error)
          (plist-get status :message) (and (not silent) text)
          (plist-get state :generation) nil)
    (when (and quiet (eq phase 'older))
      (setf (plist-get (plist-get state :page) :auto-load-paused-p) t))
    (chirp-timeline--next
     state nil
     (unless silent
       (list (chirp-timeline--feedback-command
              view state (replace-regexp-in-string "[\r\n]+" "  " text)))))))

(defun chirp-timeline--update (_context state message)
  "Reduce a data-only primary timeline MESSAGE under its exact STATE.
Retained session plists are deliberately not copied: this reducer owns their
pagination/position fields, and App tweet-result reducers own cached content."
  (if (not (eq state (nth 2 message)))
      (appkit-next-reject 'stale-timeline-state)
    (let ((view (nth 3 message))
          (data (nthcdr 4 message)))
      (pcase (nth 1 message)
        ('activate
         (chirp-timeline--reset-request state)
         (if (and (not (plist-get state :loaded-p))
                  (eq (plist-get (plist-get state :status) :phase) 'initial))
             (chirp-timeline--begin-request view state 'initial nil)
           (chirp-timeline--next
            state (or (plist-get state :position) 'first)
            (list (chirp-timeline--assets-command view state)))))
        ('request
         (pcase-let* ((`(,phase ,quiet) data)
                      ((map :generation :loaded-p :page :status) state)
                      (busy (memq (plist-get status :phase)
                                  '(initial refresh poll older))))
           (cond
            ((and (eq phase 'poll)
                  (or generation (not loaded-p)
                      (not (eq (plist-get status :phase) 'idle))))
             (appkit-next-reject 'timeline-busy))
            ((and (eq phase 'older)
                  (or busy (plist-get page :exhausted-p)
                      (not (plist-get page :next-cursor))))
             (if quiet
                 (appkit-next-reject 'no-automatic-page)
               (appkit-next
                :model state :render appkit-render-none
                :commands
                (list (chirp-timeline--feedback-command
                       view state (if busy "Timeline request already in progress..."
                                    "No older posts."))))))
            ((memq phase '(refresh poll older))
             (chirp-timeline--begin-request view state phase quiet))
            (t (appkit-next-reject 'unknown-timeline-phase)))))
        ((or 'success 'error)
         (let ((generation (car data)))
           (if (not (and generation (eq generation (plist-get state :generation))))
               (appkit-next-reject 'stale-timeline-request)
             (if (eq (nth 1 message) 'success)
                 (chirp-timeline--success-update
                  view state generation (nth 1 data) (nth 2 data))
               (chirp-timeline--error-update view state generation (nth 1 data))))))
        ('switch
         (pcase-let ((`(,target ,position) data))
           (setf (plist-get state :position) position)
           (chirp-timeline--reset-request state)
           (chirp-timeline--reset-request target)
           (let ((next
                  (if (and (not (plist-get target :loaded-p))
                           (eq (plist-get (plist-get target :status) :phase) 'initial))
                      (chirp-timeline--begin-request view target 'initial nil)
                    (chirp-timeline--next
                     target nil (list (chirp-timeline--assets-command view target))))))
             (setf (appkit-next-render next)
                   (appkit-projection-change-create
                    :full-p t :frame-p t
                    :position (or (plist-get target :position) 'first))
                   (appkit-next-commands next)
                   (append (chirp-timeline--cancel-commands)
                           (list (appkit-command-cancel-effect 'chirp-media-acquire)
                                 (appkit-command-cancel-effect 'chirp-media-present))
                           (appkit-next-commands next)))
             next)))
        ('detach
         (when (car data) (setf (plist-get state :position) (car data)))
         (dolist (feed (cadr data))
           (when-let* ((position (plist-get feed :position)))
             (setf (appkit-position-snapshot-window-snapshots position) nil)))
         (chirp-timeline--reset-request state)
         (appkit-next :model state :render appkit-render-none
                      :commands (chirp-timeline--cancel-commands)))
        ('show-pending
         (if-let* ((pending (plist-get state :pending-new-items)))
             (let ((count (length pending)))
               (setf (plist-get state :items)
                     (plist-get (chirp-timeline--merge-refreshed-tweets
                                 (plist-get state :items) pending) :tweets)
                     (plist-get state :pending-new-items) nil)
               (chirp-timeline--next
                state 'first
                (list (chirp-timeline--feedback-command
                       view state (format "Showing %d new post%s."
                                          count (if (= count 1) "" "s"))))))
           (appkit-next-reject 'no-pending-posts)))
        ('prepared (appkit-next :model state :render appkit-render-none))
        ('assets-failed
         (appkit-next
          :model state :render appkit-render-none
          :commands (list (chirp-timeline--feedback-command
                           view state (format "Timeline assets: %s" (car data))))))
        (_ (appkit-next-reject 'unknown-timeline-message))))))

(defun chirp-timeline--poll (view)
  "Check live and visible primary timeline VIEW for new posts."
  (when
      (and (appkit-surface-live-p view)
           (get-buffer-window (appkit-surface-buffer view) 'visible))
    (let*
        ((state (chirp-timeline--view-state view))
         (status (plist-get state :status)))
      (when
          (and (plist-get state :loaded-p)
               (eq (plist-get status :phase) 'idle)
               (null (plist-get state :generation)))
        (chirp-timeline--dispatch view 'request 'poll nil)))))

(defun chirp-timeline--start-polling (view)
  "Start official-style foreground polling for primary timeline VIEW."
  (when (and (numberp chirp-timeline-poll-interval)
             (> chirp-timeline-poll-interval 0))
    (let ((timer
           (run-at-time chirp-timeline-poll-interval
                        chirp-timeline-poll-interval
                        #'chirp-timeline--poll view)))
      (appkit-register-handle view 'timer timer))))

(defun chirp-timeline--open-primary (kind)
  "Open or reuse the Appkit-owned primary timeline for KIND."
  (let*
      ((app (chirp-app))
       (view (appkit-app-surface app chirp-timeline--primary-view-id)))
    (if view
        (let*
            ((state (chirp-timeline--view-state view))
             (current-kind (plist-get (plist-get state :query) :kind))
             (buffer (appkit-surface-buffer view)))
          (unless (eq current-kind kind)
            (chirp-timeline--switch-primary view kind))
          (pop-to-buffer buffer) buffer)
      (let* ((state (chirp-timeline--feed-state kind)))
        (let*
            ((view
              (chirp-open-projection-view
               :id chirp-timeline--primary-view-id
               :mode 'chirp-timeline--mode
               :title
               (chirp-timeline--title kind)
               :state state
               :render-function #'chirp-timeline--sync
               :anchor-property 'chirp-entry-id
               :setup #'chirp-timeline--setup-view
               :select t
               :ready #'chirp-timeline--surface-ready))
             (buffer (appkit-surface-buffer view)))
          (chirp-timeline--dispatch view 'activate)
          buffer)))))

(defun chirp-timeline--switch-primary (view kind)
  "Commit a switch to cached KIND, capturing the outgoing semantic position."
  (let* ((state (chirp-timeline--view-state view))
         (current-kind (plist-get (plist-get state :query) :kind)))
    (unless (eq current-kind kind)
      (chirp-timeline--dispatch
       view 'switch (chirp-timeline--feed-state kind)
       (and (plist-get state :items)
            (with-current-buffer (appkit-surface-buffer view)
              (appkit-position-capture
               :anchor-property 'chirp-entry-id :preserve-window-start t)))))))

(defun chirp-timeline--refresh-primary ()
  "Check the current Appkit-owned primary timeline for new posts."
  (if-let* ((view (chirp-timeline--current-view)))
      (chirp-timeline--dispatch view 'request 'refresh nil)
    (user-error "Current view is not a primary timeline")))

(defun chirp-timeline-show-new ()
  "Insert pending new posts and move to the newest timeline entry."
  (interactive)
  (if-let* ((view (chirp-timeline--current-view))
             ((plist-get (chirp-timeline--view-state view) :pending-new-items)))
      (chirp-timeline--dispatch view 'show-pending)
    (user-error "No new posts are waiting")))

;;; Collections

(defun chirp-timeline--likes-title (handle)
  "Return the buffer title for liked tweets by HANDLE."
  (if (and handle (not (string-empty-p handle)))
      (format "Liked: @%s" handle)
    "Liked"))

(defun chirp-timeline--list-title (list-id)
  "Return the buffer title for LIST-ID."
  (format "List: %s" list-id))

(defun chirp-timeline--list-source-labels (list-info)
  "Return human-readable source labels for LIST-INFO."
  (mapcar (lambda (source)
            (pcase source
              ("owned" "owned")
              ("subscribed" "subscribed")
              ("member" "member")
              (_ source)))
          (or (alist-get 'sources list-info) '())))

(defun chirp-timeline--list-candidate (list-info)
  "Return a minibuffer display candidate for LIST-INFO."
  (pcase-let* (((map name fullName ('id list-id) mode) list-info)
               (name (or name fullName list-id))
               (owner (map-nested-elt list-info '(owner screenName)))
               (sources (chirp-timeline--list-source-labels list-info))
         (parts (delq nil
                      (list (and owner (format "@%s" owner))
                            (and mode (not (string-empty-p mode)) mode)
                            (and sources (string-join sources ", "))
                            list-id))))
    (if parts
        (format "%s (%s)" name (string-join parts " · "))
      name)))

(defun chirp-timeline--read-list-target (lists)
  "Prompt for one accessible list from LISTS and return its id."
  (let ((choices (mapcar (lambda (list-info)
                           (cons (chirp-timeline--list-candidate list-info)
                                 (alist-get 'id list-info)))
                         lists)))
    (unless choices
      (user-error "No accessible lists found"))
    (cdr (assoc (completing-read
                 (format "List (%d): " (length choices))
                 choices
                 nil
                 t)
                choices))))

(defun chirp-timeline--prepended-new-count (current fetched)
  "Return how many unique FETCHED tweets are not already present in CURRENT.

For algorithmic timelines, especially \"For You\", the top recommendation can
stay fixed while newer posts are inserted below it.  Count every unseen tweet in
the refreshed head page so refresh feedback matches what the user will actually
see after the merge."
  (if (null current)
      0
    (let ((current-keys (make-hash-table :test #'equal))
          (seen-keys (make-hash-table :test #'equal))
          (count 0))
      (dolist (tweet current)
        (puthash (chirp-tweet-key tweet) t current-keys))
      (dolist (tweet fetched count)
        (let ((key (chirp-tweet-key tweet)))
          (unless (gethash key seen-keys)
            (puthash key t seen-keys)
            (unless (gethash key current-keys)
              (setq count (1+ count)))))))))

(defun chirp-timeline--stage-new-tweets (current pending fetched)
  "Return unseen FETCHED and PENDING tweets in newest-first order.

CURRENT contains the visible timeline.  FETCHED takes precedence over older
PENDING copies of the same tweet."
  (let ((current-keys (make-hash-table :test #'equal))
        (staged-keys (make-hash-table :test #'equal))
        staged)
    (dolist (tweet current)
      (puthash (chirp-tweet-key tweet) t current-keys))
    (dolist (tweet fetched)
      (let ((key (chirp-tweet-key tweet)))
        (unless (or (gethash key current-keys)
                    (gethash key staged-keys))
          (puthash key t staged-keys)
          (push tweet staged))))
    (dolist (tweet pending)
      (let ((key (chirp-tweet-key tweet)))
        (unless (or (gethash key current-keys)
                    (gethash key staged-keys))
          (puthash key t staged-keys)
          (push tweet staged))))
    (nreverse staged)))

(defun chirp-timeline--merge-refreshed-tweets (current fetched)
  "Return a plist describing how FETCHED should merge over CURRENT."
  (let ((merged nil)
        (merged-keys (make-hash-table :test #'equal))
        (new-count (chirp-timeline--prepended-new-count current fetched)))
    (dolist (tweet fetched)
      (let ((key (chirp-tweet-key tweet)))
        (unless (gethash key merged-keys)
          (puthash key t merged-keys)
          (push tweet merged))))
    (dolist (tweet current)
      (let ((key (chirp-tweet-key tweet)))
        (unless (gethash key merged-keys)
          (puthash key t merged-keys)
          (push tweet merged))))
    (list :tweets (nreverse merged)
          :new-count new-count)))

(defun chirp-timeline--refresh-message (new-count)
  "Return a status message for NEW-COUNT refreshed tweets."
  (if (zerop new-count)
      "No new posts."
    (format "%d new post%s."
            new-count
            (if (= new-count 1) "" "s"))))

(defun chirp-timeline--collection-state (kind title refresh)
  "Return canonical state for collection KIND titled TITLE.
REFRESH reloads the collection."
  (list :type 'collection
        :query (list :kind kind)
        :items nil
        :title title
        :refresh refresh
        :status (list :phase 'initial :message nil)
        :expanded-tweet-ids (make-hash-table :test #'equal)
        :loaded-p nil))

(defun chirp-timeline--ensure-collection (kind title refresh)
  "Open or reuse collection KIND titled TITLE with REFRESH."
  (chirp-open-projection-view
   :id (list 'collection kind title)
   :title title
   :state
   (chirp-timeline--collection-state kind
                                     title
                                     refresh)
   :render-function #'chirp-timeline--sync
   :printer #'chirp-render-print-tweet-row
   :select t))

(defun chirp-timeline--install-tweets (view tweets)
  "Install TWEETS into collection VIEW and request a projection sync."
  (let
      ((state (appkit-surface-model view))
       (buffer (appkit-surface-buffer view)))
    (setf (plist-get state :items) tweets (plist-get state :loaded-p)
          t (plist-get (plist-get state :status) :phase) 'idle
          (plist-get (plist-get state :status) :message) nil)
    (appkit-surface-post view
                         (appkit-projection-change-create
                          :position
                          (plist-get
                           (list
                            :position
                            'first)
                           :position)))
    (appkit-surface-post view
                         (appkit-projection-change-create
                          :full-p t
                          :frame-p t
                          :position 'preserve))
    nil (chirp-clear-status buffer)
    (chirp-media-prefetch-tweets tweets buffer)
    (chirp-enrich-quoted-tweets tweets buffer)))

(defun chirp-timeline--fetch-collection (view title refresh fetch-fn)
  "Run FETCH-FN for collection VIEW titled TITLE.\nREFRESH retries the request after failure."
  (let*
      ((buffer (appkit-surface-buffer view))
       (token (chirp-begin-background-request buffer title)))
    (funcall fetch-fn
             (lambda (tweets _envelope)
               (when (chirp-request-current-p buffer token)
                 (chirp-timeline--install-tweets view tweets)))
             (lambda (message)
               (when (chirp-request-current-p buffer token)
                 (chirp-show-error buffer title refresh message))))
    buffer))

;;; Primary Commands

(defun chirp-timeline-open-home ()
  "Open Chirp's unique Appkit-owned home timeline."
  (chirp-timeline--open-primary 'home))

(defun chirp-timeline-open-following ()
  "Open Chirp's unique Appkit-owned following timeline."
  (chirp-timeline--open-primary 'following))

(defun chirp-timeline--load-more-primary (view &optional quiet)
  "Request an older page for VIEW, suppressing feedback when QUIET."
  (chirp-timeline--dispatch view 'request 'older quiet))

(defun chirp-load-more (&optional _anchor-id)
  "Load older posts, preserving the current semantic position."
  (interactive)
  (cond
   ((chirp-timeline--current-view)
    (chirp-timeline--load-more-primary (chirp-timeline--current-view)))
   ((and (appkit-current-surface)
         (eq
          (plist-get (appkit-surface-model (appkit-current-surface))
                     :type)
          'profile))
    (chirp-profile-load-more))
   (t (user-error "Current view does not support loading more posts"))))

;;; Collection Commands

(defun chirp-timeline-open-bookmarks ()
  "Open bookmarks."
  (let ((refresh (lambda () (chirp-timeline-open-bookmarks))))
    (chirp-timeline--fetch-collection
     (chirp-timeline--ensure-collection 'bookmarks "Bookmarks" refresh)
     "Bookmarks" refresh #'chirp-backend-bookmarks)))

(defun chirp-timeline--open-likes-for (handle)
  "Open liked tweets for HANDLE."
  (let* ((title (chirp-timeline--likes-title handle))
         (refresh (lambda () (chirp-timeline-open-likes handle)))
         (view (chirp-timeline--ensure-collection 'likes title refresh)))
    (chirp-timeline--fetch-collection
     view title refresh
     (lambda (success errback)
       (chirp-backend-likes handle success errback)))))

(defun chirp-timeline-open-likes (&optional handle)
  "Open liked tweets for HANDLE.\n\nWhen HANDLE is nil, resolve the currently authenticated account first."
  (let
      ((clean-handle
        (and handle
             (string-remove-prefix "@"
                                   (string-trim (format "%s" handle))))))
    (if clean-handle (chirp-timeline--open-likes-for clean-handle)
      (let*
          ((title "Liked")
           (refresh (lambda () (chirp-timeline-open-likes)))
           (view
            (chirp-timeline--ensure-collection 'likes title refresh))
           (buffer (appkit-surface-buffer view))
           (token (chirp-begin-background-request buffer title)))
        (chirp-backend-whoami
         (lambda (user _envelope)
           (when (chirp-request-current-p buffer token)
             (if-let* ((resolved (plist-get user :handle)))
                 (chirp-timeline--open-likes-for resolved)
               (chirp-show-error buffer title refresh
                                 "X returned a whoami payload Chirp could not parse."))))
         (lambda (message)
           (when (chirp-request-current-p buffer token)
             (chirp-show-error buffer title refresh message))))
        buffer))))

(defun chirp-timeline-open-list (&optional list-id)
  "Open the timeline for LIST-ID.

When LIST-ID is nil, prompt from the authenticated account's lists."
  (if (null list-id)
      (let* ((buffer (chirp-buffer))
             (token (chirp-begin-request buffer)))
        (message "Loading X lists...")
        (chirp-backend-lists
         (lambda (lists _envelope)
           (when (chirp-request-current-p buffer token)
             (condition-case err
                 (progn
                   (when (buffer-live-p buffer)
                     (kill-buffer buffer))
                   (chirp-timeline-open-list
                    (chirp-timeline--read-list-target lists)))
               (quit nil)
               (user-error (message "%s" (error-message-string err))))))
         (lambda (message)
           (when (chirp-request-current-p buffer token)
             (when (buffer-live-p buffer)
               (kill-buffer buffer))
             (message "Chirp list lookup failed: %s" message))))
        buffer)
    (let ((clean-id (string-trim list-id)))
      (let* ((title (chirp-timeline--list-title clean-id))
             (refresh (lambda () (chirp-timeline-open-list clean-id))))
        (chirp-timeline--fetch-collection
         (chirp-timeline--ensure-collection 'list title refresh)
         title refresh
         (lambda (success errback)
           (chirp-backend-list clean-id success errback)))))))

;;; Search Commands

(defun chirp-timeline-open-search (query)
  "Open search results for QUERY."
  (let* ((title (format "Search: %s" query))
         (refresh (lambda () (chirp-timeline-open-search query))))
    (chirp-timeline--fetch-collection
     (chirp-timeline--ensure-collection 'search title refresh)
     title refresh
     (lambda (success errback)
       (chirp-backend-search query success errback)))))

(defun chirp-toggle-home-following ()
  "Toggle between primary Chirp subviews.

On Home/Following, switch between the two timelines.  In profile buffers that
expose subviews, cycle the current profile mode."
  (interactive)
  (cond
   ((functionp chirp--profile-switch-mode-function)
    (funcall chirp--profile-switch-mode-function :next))
   ((chirp-timeline--current-view)
    (let* ((view (chirp-timeline--current-view))
           (state (chirp-timeline--view-state view))
           (kind (plist-get (plist-get state :query) :kind)))
      (chirp-timeline--switch-primary
       view (if (eq kind 'home) 'following 'home))))
   (t
    (user-error "Current view does not support TAB switching"))))

(defun chirp-timeline--surface-ready (surface)
  "Install live timeline observers after SURFACE finishes mounting."
  (chirp-timeline--install-scroll-observer surface)
  (chirp-timeline--start-polling surface))

(provide 'chirp-timeline)

;;; chirp-timeline.el ends here
