;;; chirp-thread.el --- Thread view for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch, enrich, order, and render a focused tweet conversation.  Ancestors
;; of the focus tweet form a linear prefix chain; later replies nest as a
;; tree from that focus.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-projection)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-media)
(require 'chirp-render)
(require 'chirp-spam)

;;; Spam Rules

(defvar-local chirp-thread--refilter-function nil
  "Reapply spam rules to this thread's retained data without fetching.")

(defun chirp-thread--spam-rule-suggestion (authorp)
  "Return a spam-rule suggestion from point.

When AUTHORP is non-nil, prefer the current author's display name or handle.
Otherwise prefer the active region and then the current reply text."
  (let ((entry (chirp-entry-at-point)))
    (chirp-spam-normalize
     (cond
      (authorp
       (or (plist-get entry :author-name)
           (plist-get entry :author-handle)))
      ((use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end)))
      ((eq (plist-get entry :kind) 'tweet)
       (plist-get entry :text))))))

(defun chirp-thread-add-spam-rule (&optional authorp)
  "Persist one literal spam rule and locally refilter the current thread.
Use the active region, or current reply text, as initial input.
With prefix argument AUTHORP, suggest the author's display name or handle."
  (interactive "P")
  (let ((input (read-string "Spam phrase or keyword: "
                            (chirp-thread--spam-rule-suggestion authorp))))
    (if-let* ((rule (chirp-spam-add-rule input)))
        (progn
          (when chirp-thread--refilter-function
            (funcall chirp-thread--refilter-function))
          (message "Added spam rule: %s" rule))
      (message "Spam rule already exists: %s" (chirp-spam-normalize input)))))

(defun chirp-thread-edit-spam-rules ()
  "Open `chirp-spam-rules-file' for manual editing."
  (interactive)
  (unless (and (stringp chirp-spam-rules-file)
               (not (string-empty-p chirp-spam-rules-file)))
    (user-error "No user spam rules file is configured"))
  (let ((file (expand-file-name chirp-spam-rules-file)))
    (make-directory (file-name-directory file) t)
    (find-file file)))

;;; Discussion Model

(defun chirp-thread--key (tweet)
  "Return a stable key for TWEET."
  (or (plist-get tweet :id)
      (plist-get tweet :url)))

(defun chirp-thread--discussion-key (tweet)
  "Return the opaque discussion key for TWEET, or nil."
  (when-let* ((key (chirp-thread--key tweet)))
    (list 'tweet key)))

(defun chirp-thread--index-tweets (tweets)
  "Return a lookup table of TWEETS keyed by discussion key and id."
  (let ((by-id (make-hash-table :test #'equal)))
    (dolist (tweet tweets by-id)
      (when-let* ((key (chirp-thread--discussion-key tweet)))
        (puthash key tweet by-id)
        (when-let* ((id (plist-get tweet :id)))
          (puthash id tweet by-id))))))

(defun chirp-thread--find-tweet (tweets tweet-id)
  "Return the tweet in TWEETS whose key equals TWEET-ID, or nil."
  (and tweet-id
       (cl-find tweet-id tweets :key #'chirp-thread--key :test #'equal)))

(defun chirp-thread--ancestor-tweets (focus by-id)
  "Return FOCUS's visible ancestors from BY-ID, root first."
  (let ((current focus)
        (seen (make-hash-table :test #'equal))
        ancestors)
    (while current
      (let* ((key (chirp-thread--discussion-key current))
             (parent-id (plist-get current :reply-to-id))
             (parent (and parent-id (gethash parent-id by-id))))
        (cond
         ((or (null key) (gethash key seen))
          (setq current nil))
         (parent
          (puthash key t seen)
          (push parent ancestors)
          (setq current parent))
         (t
          (setq current nil)))))
    ancestors))

(defun chirp-thread--depth-from-focus (tweet focus by-id)
  "Return TWEET's visible depth below FOCUS using BY-ID.
Return zero when the focus is unreachable."
  (let ((current tweet)
        (seen (make-hash-table :test #'equal))
        (depth 0)
        (focus-key (chirp-thread--discussion-key focus))
        reached-p)
    (while current
      (let* ((key (chirp-thread--discussion-key current))
             (parent-id (plist-get current :reply-to-id))
             (parent (and parent-id (gethash parent-id by-id))))
        (cond
         ((or (null key) (gethash key seen))
          (setq current nil))
         ((equal key focus-key)
          (setq reached-p t
                current nil))
         (parent
          (puthash key t seen)
          (setq depth (1+ depth)
                current parent))
         (t
          (setq current nil)))))
    (if reached-p depth 0)))

(defun chirp-thread--discussion-rows (tweets &optional focus-id)
  "Return ordered Appkit discussion row data for TWEETS.

Each row contains `:key', `:parent-key', `:depth', `:role', `:connector',
`:focus-p', and `:tweet'.  Ancestors of FOCUS-ID form a depth-0 chain.
Replies after the focus nest by their distance below that focus.  When
FOCUS-ID is nil, the first renderable tweet is the focus."
  (let* ((by-id (chirp-thread--index-tweets tweets))
         (focus (or (chirp-thread--find-tweet tweets focus-id)
                    (car tweets)))
         (focus-key (and focus (chirp-thread--discussion-key focus)))
         (ancestor-keys (make-hash-table :test #'equal))
         (rows nil)
         (seen (make-hash-table :test #'equal)))
    (dolist (tweet (and focus (chirp-thread--ancestor-tweets focus by-id)))
      (when-let* ((key (chirp-thread--discussion-key tweet)))
        (puthash key t ancestor-keys)))
    (dolist (tweet tweets (nreverse rows))
      (when-let* ((key (chirp-thread--discussion-key tweet))
                  ((not (gethash key seen))))
        (let* ((focus-p (equal key focus-key))
               (chain-p (gethash key ancestor-keys))
               (parent-id (plist-get tweet :reply-to-id))
               (parent (and parent-id (gethash parent-id by-id)))
               (depth (cond
                       ((or focus-p chain-p) 0)
                       (focus (chirp-thread--depth-from-focus
                               tweet focus by-id))
                       (t 0)))
               (parent-key (and parent
                                (chirp-thread--discussion-key parent)))
               (role (cond
                      (focus-p 'focus)
                      (chain-p 'chain)
                      (t 'tree)))
               (connector (cond
                           (chain-p 'continue)
                           ((and focus-p
                                 (> (hash-table-count ancestor-keys) 0))
                            'end)
                           (t nil))))
          (puthash key t seen)
          (push (list :key key
                      :parent-key (and (or chain-p focus-p (> depth 0))
                                       parent-key)
                      :depth (if (and (eq role 'tree) parent-key)
                                 depth
                               0)
                      :role role
                      :connector connector
                      :focus-p focus-p
                      :tweet tweet)
                rows))))))

(defun chirp-thread--reorder (tweets focus-id)
  "Return TWEETS ordered around FOCUS-ID.
The ancestor chain comes first, followed by the focus and remaining replies."
  (if (not focus-id)
      tweets
    (let* ((by-id (chirp-thread--index-tweets tweets))
           (focus (chirp-thread--find-tweet tweets focus-id))
           (ancestors (and focus (chirp-thread--ancestor-tweets focus by-id)))
           (skip (make-hash-table :test #'equal)))
      (if (not focus)
          tweets
        (puthash (chirp-thread--key focus) t skip)
        (dolist (tweet ancestors)
          (puthash (chirp-thread--key tweet) t skip))
        (append ancestors
                (list focus)
                (cl-remove-if
                 (lambda (tweet)
                   (gethash (chirp-thread--key tweet) skip))
                 tweets))))))

(defun chirp-thread--filter-spam-replies (tweets &optional focus-id)
  "Hide keyword-matching replies from TWEETS.

The focus tweet and its ancestor chain are kept even when they match.
FOCUS-ID selects the focus tweet; when it is nil, the first tweet is
protected."
  (if (or (null tweets)
          (null chirp-spam-rules))
      tweets
    (let* ((rules (chirp-spam-effective-rules))
           (by-id (chirp-thread--index-tweets tweets))
           (focus (or (chirp-thread--find-tweet tweets focus-id)
                      (car tweets)))
           (protected (make-hash-table :test #'equal)))
      (when focus
        (puthash (chirp-thread--key focus) t protected)
        (dolist (tweet (chirp-thread--ancestor-tweets focus by-id))
          (puthash (chirp-thread--key tweet) t protected)))
      (cl-remove-if
       (lambda (tweet)
         (and (not (gethash (chirp-thread--key tweet) protected))
              (not (eq (plist-get tweet :timeline-context) 'related))
              (chirp-spam-match-p tweet rules)))
       tweets))))

;;; Article Enrichment

(defun chirp-thread--title (tweet-id)
  "Return a display title for TWEET-ID."
  (format "Thread: %s" tweet-id))

(defun chirp-thread--seed-tweets (tweet)
  "Return TWEET as a renderable seed list, or nil."
  (when (and tweet
             (eq (plist-get tweet :kind) 'tweet)
             (plist-get tweet :id))
    (list tweet)))

(defun chirp-thread--article-fetch-needed-p (tweet)
  "Return non-nil when TWEET needs direct article enrichment."
  (and (plist-get tweet :id)
       (not (chirp-first-nonblank (plist-get tweet :article-text)))
       (or (chirp-first-nonblank (plist-get tweet :article-title))
           (and (string-empty-p (or (plist-get tweet :text) ""))
                (plist-get tweet :urls)))))

(defun chirp-thread--maybe-apply-article (tweets article-tweet)
  "Return TWEETS with ARTICLE-TWEET replacing the matching tweet when ids match."
  (if (and tweets article-tweet)
      (mapcar (lambda (tweet)
                (if (equal (plist-get tweet :id)
                           (plist-get article-tweet :id))
                    article-tweet
                  tweet))
              tweets)
    tweets))

;;; Rendering

(defun chirp-thread--print-row (row)
  "Insert one projected discussion ROW."
  (chirp-render-insert-discussion-entry
   (appkit-projection-row-payload row)))

(defun chirp-thread--project-rows (tweets focus-id)
  "Project TWEETS into keyed discussion rows for FOCUS-ID."
  (appkit-projection-project
   (chirp-thread--discussion-rows tweets focus-id)
   (lambda (row) (plist-get row :key))
   :dependencies-function
   (lambda (row)
     (chirp-render--tweet-row-dependencies (plist-get row :tweet)))))

(defun chirp-thread--frame-text (state)
  "Return header text representing thread STATE."
  (let* ((status (plist-get state :status))
         (phase (plist-get status :phase))
         (message (plist-get status :message)))
    (pcase phase
      ('initial "Loading thread...\n\n")
      ('error (format "Unable to load data.\n\n%s\n\n" message))
      (_ (and (null (plist-get state :items))
              "No thread data returned.\n")))))

(defun chirp-thread--sync (surface _app state change)
  "Render committed STATE in SURFACE using native projection CHANGE."
  (chirp-render-projection
      surface change
    (chirp-thread--project-rows (plist-get state :items)
                                (plist-get (plist-get state :query) :focus-id))
    (chirp-thread--frame-text state)))

(defun chirp-thread--ensure-view (title refresh focus-id &optional id)
  "Open or reuse a thread view titled TITLE focused on FOCUS-ID.\nREFRESH reloads the thread; optional ID overrides its Appkit identity."
  (chirp-open-projection-view
   :id
   (or id (list 'thread title focus-id))
   :title title
   :state
   (list :type 'thread :query
         (list :focus-id focus-id) :items
         nil :all-items nil :title title :refresh refresh
         :status
         (list :phase 'initial :message nil)
         :expanded-tweet-ids
         (make-hash-table :test #'equal))
   :render-function #'chirp-thread--sync
   :printer #'chirp-thread--print-row
   :select t))

(defun chirp-thread--present (view tweets &optional position)
  "Install TWEETS into thread VIEW and request a projection sync."
  (let
      ((state (appkit-surface-model view))
       (buffer (appkit-surface-buffer view)))
    (setf (plist-get state :items) tweets
          (plist-get (plist-get state :status) :phase) 'idle
          (plist-get (plist-get state :status) :message) nil)
    (appkit-surface-post view
                         (appkit-projection-change-create
                          :position
                          (plist-get
                           (list
                            :position
                            (or
                             position
                             'first))
                           :position)))
    (appkit-surface-post view
                         (appkit-projection-change-create
                          :full-p t
                          :frame-p t
                          :position 'preserve))
    nil (chirp-media-prefetch-tweets tweets buffer)
    (chirp-enrich-quoted-tweets tweets buffer)))

;;; Commands

(defun chirp-thread-open (tweet-id)
  "Open a thread focused on TWEET-ID."
  (chirp-thread--open tweet-id))

(defun chirp-thread-open-tweet (tweet)
  "Open a thread focused on normalized TWEET."
  (unless (and (listp tweet)
               (eq (plist-get tweet :kind) 'tweet))
    (user-error "Need a normalized tweet"))
  (chirp-thread--open (plist-get tweet :id) tweet))

(defun chirp-thread--open (tweet-id &optional seed-tweet)
  "Open TWEET-ID, optionally rendering normalized SEED-TWEET first."
  (let*
      ((title (chirp-thread--title tweet-id))
       (refresh
        (lambda () (chirp-backend-invalidate-thread tweet-id)
          (chirp-thread-open tweet-id)))
       (view
        (chirp-thread--ensure-view title refresh tweet-id
                                   (list 'thread tweet-id)))
       (state (appkit-surface-model view))
       (buffer (appkit-surface-buffer view))
       (prefetched-article nil) (article-requested-p nil) (token nil))
    (cl-labels
        ((present-current (&optional position)
           (chirp-thread--present
            view (chirp-thread--filter-spam-replies
                  (plist-get state :all-items) tweet-id)
            position))
         (apply-prefetched-article nil
           (setf (plist-get state :all-items)
                 (chirp-thread--maybe-apply-article
                  (plist-get state :all-items) prefetched-article)))
         (handle-article-success (article-tweet _envelope)
           (when (chirp-request-current-p buffer token)
             (setq prefetched-article article-tweet)
             (when (plist-get state :all-items)
               (apply-prefetched-article) (present-current 'preserve)
               (chirp-clear-status buffer))))
         (maybe-request-article (tweet)
           (when
               (and (not article-requested-p)
                    (chirp-thread--article-fetch-needed-p tweet))
             (setq article-requested-p t)
             (chirp-set-status buffer
                               "Thread ready · loading article...")
             (chirp-backend-article (plist-get tweet :id)
                                    #'handle-article-success
                                    (lambda (_message)
                                      (when
                                          (chirp-request-current-p
                                           buffer token)
                                        (chirp-clear-status buffer)))))))
      (setq token (chirp-begin-background-request buffer title))
      (with-current-buffer buffer
        (setq-local chirp-thread--refilter-function
                    (lambda ()
                      (when (and (plist-get state :all-items)
                                 (chirp-request-current-p buffer token))
                        (present-current 'preserve)))))
      (when-let* ((seed (chirp-thread--seed-tweets seed-tweet)))
        (setf (plist-get state :all-items) seed)
        (present-current (list 'tweet tweet-id)))
      (when seed-tweet (maybe-request-article seed-tweet))
      (chirp-backend-thread tweet-id
                            (lambda (tweets _envelope)
                              (when
                                  (chirp-request-current-p buffer
                                                           token)
                                (setf (plist-get state :all-items)
                                      (chirp-thread--reorder tweets tweet-id))
                                (apply-prefetched-article)
                                (present-current
                                 (list 'tweet tweet-id))
                                (if-let*
                                    ((focus
                                      (or
                                       (chirp-thread--find-tweet
                                        (plist-get state :all-items) tweet-id)
                                       (car (plist-get state :all-items)))))
                                    (progn
                                      (maybe-request-article focus)
                                      (unless article-requested-p
                                        (chirp-clear-status buffer)))
                                  (chirp-clear-status buffer))))
                            (lambda (message)
                              (when
                                  (chirp-request-current-p buffer
                                                           token)
                                (chirp-show-error buffer title refresh
                                                  message))))
      buffer)))

(provide 'chirp-thread)

;;; chirp-thread.el ends here
