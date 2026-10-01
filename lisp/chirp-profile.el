;;; chirp-profile.el --- Profile view for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch and render profile summaries, timelines, and account lists.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'appkit-projection)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)
(require 'chirp-timeline)

;;; Constants

(defconst chirp-profile--base-modes '(posts replies highlights media)
  "Profile subviews shown for all profiles.")

;;; Variables

(defvar-local chirp-profile--user nil
  "Buffer-local cached profile plist for the active profile view.")
(defvar-local chirp-profile--tweets nil
  "Buffer-local cached tweet list for the active profile view.")
(defvar-local chirp-profile--available-modes chirp-profile--base-modes
  "Buffer-local list of available profile subview modes.")

;;; Labels and Projection

(defun chirp-profile--list-title (kind handle)
  "Return a title for KIND list belonging to HANDLE."
  (format "%s: @%s"
          (pcase kind
            ('followers "Followers")
            ('following "Following")
            (_ "Users"))
          (string-remove-prefix "@" handle)))

(defun chirp-profile--mode-label (mode)
  "Return a human-readable label for profile MODE."
  (pcase mode
    ('posts "Posts")
    ('replies "Replies")
    ('highlights "Highlights")
    ('media "Media")
    ('likes "Likes")
    (_ (capitalize (symbol-name mode)))))

(defun chirp-profile--paginated-mode-p (mode)
  "Return non-nil when profile MODE supports cursor-based pagination."
  (memq mode '(posts replies highlights media)))

(defun chirp-profile--title (handle mode)
  "Return a title for HANDLE profile subview MODE."
  (let ((base (format "@%s" (string-remove-prefix "@" handle))))
    (if (eq mode 'posts)
        base
      (format "%s · %s" base (chirp-profile--mode-label mode)))))

(defun chirp-profile--next-mode (current modes)
  "Return the mode following CURRENT inside MODES."
  (let* ((modes (or modes '(posts)))
         (index (or (cl-position current modes) 0)))
    (nth (mod (1+ index) (length modes)) modes)))

(defun chirp-profile--print-user-row (row)
  "Insert one projected user ROW."
  (chirp-render-insert-user-summary (appkit-projection-row-payload row)))

(defun chirp-profile--project-users (users)
  "Project USERS into keyed rows."
  (appkit-projection-project
   users
   (lambda (user)
     (map-let (:id :handle) user
       (list 'user (or id handle))))))

(defun chirp-profile--header-text (state)
  "Return generated profile chrome and empty/error text for STATE."
  (map-let (:user :mode :modes :items :timeline-ready
                  (:status (map :phase :message))) state
    (with-temp-buffer
      (when user
        (chirp-render-insert-user-summary user)
        (chirp-render-insert-profile-view-strip mode modes))
      (cond
       (items nil)
       ((eq phase 'error)
        (insert (or message "Unable to load data.") "\n"))
       (timeline-ready
        (insert (format "No %s returned.\n"
                        (downcase (chirp-profile--mode-label mode)))))
       (user
        (insert (propertize
                 (format "Loading %s..."
                         (downcase (chirp-profile--mode-label mode)))
                 'face 'shadow)
                "\n")))
      (buffer-string))))

(defun chirp-profile--sync (surface _app state change)
  "Render committed STATE in SURFACE using native projection CHANGE."
  (chirp-profile--bind-locals surface)
  (chirp-render-projection
      surface change
    (chirp-render-project-tweet-rows (map-elt state :items))
    (chirp-profile--header-text state)))

(defun chirp-profile--users-sync (surface _app state change)
  "Render committed user-list STATE using native projection CHANGE."
  (map-let (:items :title (:status (map :phase :message))) state
    (chirp-render-projection
        surface change (chirp-profile--project-users items)
        (pcase phase
          ('initial (format "Loading %s...\n" title))
          ('error (format "Unable to load data.\n\n%s\n" message))
          (_ (and (null items) "No users returned.\n"))))))

(defun chirp-profile--bind-locals (view)
  "Mirror VIEW state into the profile buffer's command locals."
  (pcase-let* ((buffer (appkit-surface-buffer view))
               ((map :user :items :mode :modes :timeline-ready :loading-more
                     (:page (map :next-cursor))) (appkit-surface-model view)))
    (with-current-buffer buffer
      (setq-local chirp-profile--user user
                  chirp-profile--tweets items
                  chirp--profile-view-mode mode
                  chirp-profile--available-modes modes
                  chirp--profile-handle (map-elt user :handle)
                  chirp--profile-switch-mode-function
                  (lambda (target)
                    (chirp-profile--switch-mode chirp--profile-handle
                                                target buffer))
                  chirp--timeline-count (length items)
                  chirp--timeline-next-cursor next-cursor
                  chirp--timeline-load-more-function
                  (and timeline-ready (chirp-profile--paginated-mode-p mode)
                       next-cursor #'chirp-profile-load-more)
                  chirp--timeline-exhausted-p (and timeline-ready (not next-cursor))
                  chirp--timeline-loading-more loading-more))))

;;; Relationship Updates

(defun chirp-profile--follow-update (model handle following payload)
  "Commit HANDLE's confirmed relationship to MODEL and redraw only its summary.
FOLLOWING is the successful action's state; REST PAYLOAD fields take precedence.
This runs inside the owning Surface's update, never in a transport callback."
  (map-let (:type :user :items) model
    (let* ((handle (chirp-backend--normalize-handle handle))
           (profile-p (eq type 'profile))
           (users (pcase type
                    ('profile (list user))
                    ('users items)))
           (confirmed (assq 'following payload))
           (followers (assq 'followers_count payload))
           (followed-by (assq 'followed_by payload))
           (following (if confirmed
                          (chirp-boolean-value (cdr confirmed))
                        following))
           keys)
      (dolist (user users)
        (map-let (:id (:handle user-handle)
                      (:viewer-following-p old-state)
                      (:followers follower-count)) user
          (when (and user
                     (equal handle
                            (chirp-backend--normalize-handle user-handle)))
            (plist-put user :viewer-following-p following)
            (cond
             (followers
              (plist-put user :followers (cdr followers)))
             ((not (eq old-state following))
              (plist-put user :followers
                         (chirp-adjust-count follower-count
                                             (if following 1 -1)))))
            (when followed-by
              (plist-put user :viewer-followed-by-p
                         (chirp-boolean-value (cdr followed-by))))
            (push (list 'user (or id user-handle)) keys))))
      (if keys
          (appkit-next
           :model model
           :render (appkit-projection-change-create
                    :keys (unless profile-p keys)
                    :frame-p profile-p
                    :position 'preserve))
        (appkit-next-reject 'unrelated-user)))))

;;; Views

(defun chirp-profile--ensure-view (handle title refresh mode)
  "Open or reuse HANDLE's profile view titled TITLE.\nREFRESH reloads the selected MODE."
  (chirp-open-projection-view
   :id (list 'profile handle)
   :title title
   :state
   (list :type 'profile :query
         (list :handle handle :mode mode)
         :user nil :items nil :mode mode
         :modes chirp-profile--base-modes
         :title title :refresh refresh
         :page (list :next-cursor nil)
         :status
         (list :phase 'initial :message nil)
         :timeline-ready nil :loading-more nil :profile-request nil
         :expanded-tweet-ids (make-hash-table :test #'equal))
   :render-function #'chirp-profile--sync
   :printer #'chirp-render-print-tweet-row
   :select t))

(defun chirp-profile--present (view)
  "Request a projection update for profile VIEW."
  (appkit-surface-post view
                       (appkit-projection-change-create
                        :full-p t
                        :frame-p t
                        :position 'preserve))
  nil)

(defun chirp-profile--open-user-list (kind handle)
  "Open KIND user list for HANDLE."
  (let*
      ((clean-handle (string-remove-prefix "@" handle))
       (title (chirp-profile--list-title kind clean-handle))
       (refresh
        (lambda () (chirp-backend-invalidate-user clean-handle)
          (chirp-profile--open-user-list kind clean-handle)))
       (view
        (chirp-open-projection-view
         :id
         (list 'users kind clean-handle)
         :title title
         :state
         (list :type 'users :query
               (list :kind kind :handle
                     clean-handle)
               :items nil :title title
               :refresh refresh :status
               (list :phase 'initial
                     :message nil)
               :wrap-navigation nil)
         :render-function #'chirp-profile--users-sync
         :printer #'chirp-profile--print-user-row
         :select t))
       (buffer (appkit-surface-buffer view))
       (fetch-fn
        (pcase kind
          ('followers #'chirp-backend-followers)
          ('following #'chirp-backend-following-users)
          (_ (error "Unknown profile list kind: %S" kind))))
       (token (chirp-begin-background-request buffer title)))
    (funcall fetch-fn clean-handle
             (lambda (users _envelope)
               (when (chirp-request-current-p buffer token)
                 (let ((state (appkit-surface-model view)))
                   (setf (plist-get state :items) users
                         (plist-get (plist-get state :status) :phase)
                         'idle)
                   (appkit-surface-post view
                                        (appkit-projection-change-create
                                         :full-p t
                                         :frame-p t
                                         :position 'preserve))
                   nil (chirp-clear-status buffer)
                   (dolist (user users)
                     (chirp-media-prefetch-user user buffer)))))
             (lambda (message)
               (when (chirp-request-current-p buffer token)
                 (chirp-show-error buffer title refresh message))))
    buffer))

;;; Pagination

(defun chirp-profile-load-more (&optional _anchor-id)
  "Load older items for the current profile view."
  (interactive)
  (pcase-let* ((view (chirp--live-projection-view))
               (state (and view (appkit-surface-model view)))
               ((map :type :loading-more (:user (map :handle))
                     (:page (map :next-cursor))) state))
    (unless (and state (eq type 'profile) handle)
      (user-error "Current view does not support loading more items"))
    (cond
     (loading-more
      (message "Already loading older posts..."))
     ((not next-cursor)
      (message "No older posts."))
     (t
      (let* ((buffer (appkit-surface-buffer view))
             (saved-mode (map-elt state :mode))
             (mode-label (downcase (chirp-profile--mode-label saved-mode)))
             (token (chirp-begin-request buffer)))
        (setf (plist-get state :loading-more) t)
        (chirp-set-status buffer (format "Loading older %s..." mode-label))
        (message "Loading older %s..." mode-label)
        (chirp-profile--fetch-content
         saved-mode handle
         (lambda (tweets envelope)
           (when (chirp-request-current-p buffer token)
             (map-let ((:items current) :page) state
               (let* ((next-cursor (chirp-backend-envelope-next-cursor envelope))
                      (merged (chirp-append-unique-tweets current tweets))
                      (added-p (> (length merged) (length current))))
                 (setf (plist-get state :items) merged
                       (plist-get page :next-cursor) next-cursor
                       (plist-get state :timeline-ready) t
                       (plist-get state :loading-more) nil)
                 (appkit-surface-post
                  view (appkit-projection-change-create :position 'preserve))
                 (chirp-profile--present view)
                 (chirp-clear-status buffer)
                 (chirp-media-prefetch-tweets tweets buffer)
                 (chirp-enrich-quoted-tweets tweets buffer)
                 (unless added-p
                   (message "No older %s." mode-label))))))
         (lambda (message)
           (when (chirp-request-current-p buffer token)
             (setf (plist-get state :loading-more) nil)
             (chirp-set-status buffer "Load more failed" 'error)
             (message "%s" (replace-regexp-in-string "[\r\n]+" "  " message))))
         chirp-profile-post-limit next-cursor))))))

;;; Requests

(defun chirp-profile--fetch-content (mode handle callback errback &optional max-results cursor)
  "Fetch profile MODE content for HANDLE and call CALLBACK.

Call ERRBACK on failure.  MAX-RESULTS limits the response.  CURSOR is only used
for paginated modes."
  (pcase mode
    ('posts
     (chirp-backend-user-posts handle callback errback max-results cursor))
    ('replies
     (chirp-backend-user-replies handle callback errback max-results cursor))
    ('highlights
     (chirp-backend-user-highlights handle callback errback max-results cursor))
    ('media
     (chirp-backend-user-media handle callback errback max-results cursor))
    ('likes
     (chirp-backend-likes handle callback errback))
    (_
     (funcall (or errback
                  (lambda (message)
                    (message "%s" message)))
              (format "Unsupported profile mode: %S" mode)))))

(defun chirp-profile--switch-mode (handle target &optional buffer)
  "Switch HANDLE profile BUFFER to TARGET mode.

When TARGET is `:next', cycle through the available profile modes."
  (let* ((buffer (or buffer (current-buffer)))
         (mode (with-current-buffer buffer
                 (if (eq target :next)
                     (chirp-profile--next-mode
                      chirp--profile-view-mode
                      chirp-profile--available-modes)
                   target))))
    (unless (eq mode (with-current-buffer buffer chirp--profile-view-mode))
      (chirp-profile-open handle mode))))

;;; Commands

(defun chirp-profile-open (handle &optional mode)
  "Open HANDLE's profile.\n\nMODE selects the active profile subview and defaults to `posts'."
  (let*
      ((clean-handle (string-remove-prefix "@" handle))
       (mode (or mode 'posts))
       (title (chirp-profile--title clean-handle mode))
       (refresh
        (lambda () (chirp-backend-invalidate-user clean-handle)
          (chirp-profile-open clean-handle mode)))
       (view
        (chirp-profile--ensure-view clean-handle title refresh mode))
       (state (appkit-surface-model view))
       (buffer (appkit-surface-buffer view)) (token nil)
       (saved-user nil) (saved-tweets nil)
       (available-modes chirp-profile--base-modes)
       (timeline-next-cursor nil) (user-ready nil)
       (timeline-ready nil) (whoami-ready nil)
       (timeline-error-message nil) (content-prefetched nil))
    (cl-labels
        ((present-current nil
           (when user-ready
             (setf (plist-get state :user) saved-user
                   (plist-get state :mode) mode
                   (plist-get state :modes) available-modes
                   (plist-get state :title) title
                   (plist-get state :refresh) refresh)
             (when (and timeline-ready
                        (not (map-elt state :timeline-ready)))
               (map-let (:page :status) state
                 (setf (plist-get state :items) saved-tweets
                       (plist-get state :timeline-ready) t
                       (plist-get page :next-cursor) timeline-next-cursor
                       (plist-get status :phase)
                       (if timeline-error-message 'error 'idle)
                       (plist-get status :message) timeline-error-message)))
             (chirp-profile--present view))
           (when
               (and user-ready timeline-ready (not content-prefetched))
             (setq content-prefetched t)
             (when saved-tweets
               (chirp-media-prefetch-tweets saved-tweets buffer)
               (chirp-enrich-quoted-tweets saved-tweets buffer)))
           (when (and user-ready timeline-ready whoami-ready)
             (setf (plist-get state :profile-request) nil)
             (with-current-buffer buffer
               (when (eq chirp--request-token token)
                 (setq-local chirp--request-token nil)))))
         (current-request-p nil
           (chirp-view-state-token-current-p
            view state token :profile-request))
         (update-status nil
           (unless (map-elt state :loading-more)
             (cond
              (timeline-error-message
               (chirp-set-status buffer
				 (format "%s failed"
					 (chirp-profile--mode-label mode))
				 'error))
              ((and user-ready timeline-ready)
               (chirp-clear-status buffer))
              (user-ready
               (chirp-set-status buffer
				 (format "Profile ready · loading %s..."
					 (downcase
                                          (chirp-profile--mode-label
                                           mode)))))
              (timeline-ready
               (chirp-set-status buffer
				 (format "%s ready · loading profile..."
					 (chirp-profile--mode-label mode))))))))
      (setq token (chirp-begin-background-request buffer title))
      (setf (plist-get state :profile-request) token)
      (chirp-set-status buffer "Loading profile...")
      (chirp-backend-user clean-handle
                          (lambda (user _envelope)
                            (when
                                (current-request-p)
                              (setq saved-user user user-ready t)
                              (plist-put saved-user :self-p
                                         (memq 'likes available-modes))
                              (update-status) (present-current)
                              (chirp-media-prefetch-user saved-user
                                                         buffer)))
                          (lambda (message)
                            (when
                                (current-request-p)
                              (setf (plist-get state :profile-request) nil)
                              (with-current-buffer buffer
                                (setq-local chirp--request-token nil))
                              (chirp-show-error buffer title refresh
                                                message))))
      (chirp-backend-whoami
       (lambda (user _envelope)
         (when (current-request-p)
           (let*
               ((self-handle (map-elt user :handle))
                (modes
                 (if
                     (and self-handle
                          (string-equal
                           (downcase
                            (string-remove-prefix "@" self-handle))
                           (downcase clean-handle)))
                     (append chirp-profile--base-modes '(likes))
                   chirp-profile--base-modes)))
             (setq available-modes modes whoami-ready t)
             (when saved-user
               (plist-put saved-user :self-p (memq 'likes modes)))
             (when (not (memq mode modes))
               (setq mode 'posts title
                     (chirp-profile--title clean-handle mode)))
             (when user-ready (update-status) (present-current)))))
       (lambda (_message)
         (when (current-request-p)
           (setq whoami-ready t) (present-current))))
      (chirp-profile--fetch-content mode clean-handle
                                    (lambda (tweets timeline-envelope)
                                      (when
                                          (current-request-p)
                                        (setq saved-tweets tweets
                                              timeline-ready t
                                              timeline-error-message
                                              nil timeline-next-cursor
                                              (and
                                               (chirp-profile--paginated-mode-p
                                                mode)
                                               (chirp-backend-envelope-next-cursor
                                                timeline-envelope)))
                                        (update-status)
                                        (present-current)))
                                    (lambda (message)
                                      (when
                                          (current-request-p)
                                        (setq saved-tweets nil
                                              timeline-ready t
                                              timeline-next-cursor nil
                                              timeline-error-message
                                              (format
                                               "Unable to load %s.\n\n%s"
                                               (downcase
                                                (chirp-profile--mode-label
                                                 mode))
                                               message))
                                        (update-status)
                                        (present-current)))
                                    chirp-profile-post-limit)
      buffer)))

(defun chirp-profile-open-followers (handle)
  "Open followers for HANDLE."
  (chirp-profile--open-user-list 'followers handle))

(defun chirp-profile-open-following-users (handle)
  "Open followed accounts for HANDLE."
  (chirp-profile--open-user-list 'following handle))

(provide 'chirp-profile)

;;; chirp-profile.el ends here
