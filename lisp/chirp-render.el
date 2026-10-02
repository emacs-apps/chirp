;;; chirp-render.el --- Rendering helpers for chirp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Shared text, link, tweet, and media rendering for Chirp view buffers.

;;; Code:

(declare-function appkit-media-video-inline-muted-p
                  "appkit-media-resource" (surface))

(declare-function nerd-icons-faicon "nerd-icons" (icon-name &rest args))
(declare-function nerd-icons-mdicon "nerd-icons" (icon-name &rest args))
(declare-function chirp-profile-open "chirp-profile" (handle &optional mode))
(declare-function chirp-profile-open-followers "chirp-profile" (handle))
(declare-function chirp-profile-open-following-users "chirp-profile" (handle))
(declare-function chirp-timeline-open-search "chirp-timeline" (query))
(declare-function chirp-toggle-follow-user-at-point "chirp-actions" ())
(declare-function chirp-reply-at-point "chirp-actions" ())
(declare-function chirp-toggle-retweet-at-point "chirp-actions" ())
(declare-function chirp-toggle-like-at-point "chirp-actions" ())
(declare-function chirp-toggle-bookmark-at-point "chirp-actions" ())
(declare-function chirp-quote-at-point "chirp-actions" ())
(declare-function chirp-poll-vote "chirp-actions" (tweet choice-id))
(declare-function chirp-poll-pending-p "chirp-actions" (tweet-id))
(declare-function chirp-edit-history-open-tweet "chirp-edit-history" (tweet))
(declare-function chirp-media-open-dedicated "chirp-media-view"
                  (selection &optional title buffer external-fallback))

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'chirp-translate)
(require 'appkit-chat-avatar)
(require 'appkit-chat-ins)
(require 'appkit-projection)
(require 'appkit-discussion)
(require 'appkit-media-image)
(require 'video)
(require 'appkit-ui)
(require 'chirp-core)
(require 'chirp-time)
(require 'chirp-media)
(require 'chirp-media-view)
(require 'chirp-media-layout)
(require 'nerd-icons nil t)

;;; Options

(defcustom chirp-tweet-separator "- - - - - - - - - - - -"
  "Separator text inserted between tweets in list views.

Set this to nil or an empty string to disable tweet separators."
  :type '(choice (const :tag "No separator" nil)
                 (string :tag "Separator text"))
  :group 'chirp)

(defcustom chirp-tweet-separator-indent 6
  "Number of leading spaces before tweet list separators."
  :type 'integer
  :group 'chirp)

;;; Constants

(defconst chirp-render-list-reply-prefix "  "
  "Indentation used for direct replies to the previous visible tweet.")

(defconst chirp-render--reply-control-labels
  '(("byinvitation" . "Accounts %s mentioned can reply")
    ("community" . "Accounts %s follows or mentioned can reply")
    ("followers" . "Accounts following or mentioned by %s can reply")
    ("mynetwork" . "Accounts %s follows, who they follow, or mentioned can reply")
    ("subscribers" . "Accounts subscribed to or mentioned by %s can reply")
    ("verified" . "Verified accounts or accounts mentioned by %s can reply"))
  "Known X reply-control modes and their display templates.")

;;; Faces

(defface chirp-author-face
  '((t :inherit (bold font-lock-keyword-face)))
  "Face used for author names."
  :group 'chirp)

(defface chirp-handle-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for handles."
  :group 'chirp)

(defface chirp-meta-face
  '((t :inherit shadow))
  "Face used for metadata."
  :group 'chirp)

(defface chirp-link-face
  '((t :inherit link))
  "Face used for links and inline @mentions, #hashtags, and $cashtags."
  :group 'chirp)

(defface chirp-hashtag-face
  '((t :inherit chirp-link-face))
  "Face used for hashtags and cashtags."
  :group 'chirp)

(defface chirp-article-title-face
  '((t :inherit (bold font-lock-doc-face)))
  "Face used for article titles."
  :group 'chirp)

(defface chirp-article-summary-face
  '((t :inherit font-lock-doc-face))
  "Face used for short article previews."
  :group 'chirp)

(defface chirp-link-card-title-face
  '((t :inherit (bold font-lock-doc-face)))
  "Face used for external link-card titles."
  :group 'chirp)

(defface chirp-link-card-description-face
  '((t :inherit shadow))
  "Face used for external link-card descriptions."
  :group 'chirp)

(defface chirp-quoted-tweet-border-face
  '((t :inherit shadow))
  "Face used for the display-only border of Appkit cards."
  :group 'chirp)

(defface chirp-quoted-tweet-face
  '((t :inherit (bold font-lock-doc-face)))
  "Face used for quoted-tweet headers."
  :group 'chirp)

(defface chirp-media-placeholder-face
  '((t :inherit shadow :box t))
  "Face used for text media placeholders."
  :group 'chirp)

(defface chirp-thread-reply-context-face
  '((t :inherit shadow :slant italic))
  "Face used for reply context lines in thread views."
  :group 'chirp)

(defface chirp-thread-related-context
  '((t :inherit (bold font-lock-keyword-face)))
  "Face used for related-tweet context lines in thread views."
  :group 'chirp)

(defface chirp-social-context-face
  '((t :inherit shadow))
  "Face used for home/following social context lines."
  :group 'chirp)

(defface chirp-thread-divider-face
  '((t :inherit shadow))
  "Face used for separators inside thread views."
  :group 'chirp)

(defface chirp-tweet-separator-face
  '((t :inherit chirp-thread-divider-face))
  "Face used for separators between tweet list entries."
  :group 'chirp)

(defface chirp-profile-view-active-face
  '((t :inherit (mode-line-emphasis link)))
  "Face used for the active profile subview label."
  :group 'chirp)

(defface chirp-profile-view-inactive-face
  '((t :inherit shadow))
  "Face used for inactive profile subview labels."
  :group 'chirp)

(defface chirp-profile-action-face
  '((t :inherit button))
  "Face used for clickable profile action buttons."
  :group 'chirp)

(defface chirp-profile-action-secondary-face
  '((t :inherit shadow))
  "Face used for secondary profile relationship labels."
  :group 'chirp)

(defface chirp-active-metric-face
  '((t :inherit (bold success)))
  "Face used for active tweet state metrics."
  :group 'chirp)

(defface chirp-poll-option-face
  '((t :inherit button))
  "Face used for available poll choices."
  :group 'chirp)

(defface chirp-poll-winner-face
  '((t :inherit (bold success)))
  "Face used for poll choices tied for the most votes."
  :group 'chirp)

(defface chirp-poll-selected-face
  '((t :inherit (bold font-lock-keyword-face)))
  "Face used for the viewer's poll choice."
  :group 'chirp)

(defface chirp-liked-metric-face
  '((((class color) (background light))
     :inherit bold
     :foreground "#d73a49")
    (((class color) (background dark))
     :inherit bold
     :foreground "#ff7b8b")
    (t :inherit chirp-active-metric-face))
  "Face used for liked tweet metrics."
  :group 'chirp)

(defface chirp-retweeted-metric-face
  '((((class color) (background light))
     :inherit bold
     :foreground "#1f9d55")
    (((class color) (background dark))
     :inherit bold
     :foreground "#4ddf83")
    (t :inherit chirp-active-metric-face))
  "Face used for retweeted tweet metrics."
  :group 'chirp)

(defface chirp-bookmarked-metric-face
  '((((class color) (background light))
     :inherit bold
     :foreground "#2563eb")
    (((class color) (background dark))
     :inherit bold
     :foreground "#6ea8ff")
    (t :inherit chirp-active-metric-face))
  "Face used for bookmarked tweet metrics."
  :group 'chirp)

;;; Text Properties and Actions

(defun chirp-render--metric-face (label active)
  "Return the face used for metric LABEL.

When ACTIVE is non-nil, prefer the action-specific face for LABEL."
  (if active
      (pcase label
        ('like 'chirp-liked-metric-face)
        ('retweet 'chirp-retweeted-metric-face)
        ('bookmark 'chirp-bookmarked-metric-face)
        (_ 'chirp-active-metric-face))
    'chirp-meta-face))

(defun chirp-render--entry-key (entry)
  "Return a stable domain key for ENTRY, or nil."
  (pcase (plist-get entry :kind)
    ('tweet
     (when-let* ((key (chirp-tweet-key entry)))
       (list 'tweet key)))
    ('user
     (when-let* ((handle (plist-get entry :handle)))
       (list 'user handle)))))

(cl-defun chirp-render--add-action (start end action &key help-echo properties face)
  "Make START..END an Appkit action that calls ACTION.

HELP-ECHO describes that action.  PROPERTIES are extra text properties
stored on the same span.  FACE is appended when non-nil."
  (when (and (functionp action)
             (< start end))
    (when properties
      (add-text-properties start end properties))
    (appkit-ui-add-action start end action
                          :help-echo help-echo
                          :face face)))

(cl-defun chirp-render--add-fallback-action (start end action &key help-echo)
  "Add ACTION on START..END only where no Appkit action exists yet.

Nested author, entity, media, and metric spans keep the actions they
already installed."
  (when (and (functionp action)
             (< start end))
    (let ((pos start))
      (while (< pos end)
        (let ((next (or (next-single-property-change
                         pos appkit-ui-action-property nil end)
                        end)))
          (unless (get-text-property pos appkit-ui-action-property)
            (chirp-render--add-action pos next action
                                      :help-echo help-echo))
          (setq pos next))))))

(defun chirp-render--mark-entry (start end entry)
  "Mark the region from START to END as ENTRY."
  (when (< start end)
    (let ((key (chirp-render--entry-key entry)))
      (add-text-properties
       start end
       (append
        `(chirp-entry-item ,entry
          chirp-entry-url
          ,(or (plist-get entry :url)
               (plist-get entry :profile-url))
          rear-nonsticky t)
        (when key `(chirp-entry-id ,key)))))
    (put-text-property start (1+ start) 'chirp-entry-start t)))

(defun chirp-render--mark-url-region (start end url)
  "Mark the region from START to END as opening URL in a browser."
  (when (and (stringp url)
             (not (string-empty-p url)))
    (chirp-render--add-action
     start end
     (lambda ()
       (browse-url url))
     :help-echo (format "Open %s" url)
     :properties `(chirp-subentry-url ,url))))

(defun chirp-render--add-profile-action (start end handle &optional help-echo)
  "Make START..END open HANDLE's profile.

HELP-ECHO defaults to a short Open-profile description."
  (when handle
    (let ((clean (string-remove-prefix "@" handle)))
      (chirp-render--add-action
       start end
       (lambda ()
         (chirp-open-profile-handle clean))
       :help-echo (or help-echo (format "Open @%s" clean))
       :properties
       `(chirp-author-handle ,clean
         chirp-author-profile-url
         ,(format "https://x.com/%s" clean))))))

(defun chirp-render--mark-profile-list-region (start end kind handle)
  "Mark the region from START to END as profile list KIND for HANDLE."
  (when handle
    (chirp-render--add-action
     start end
     (lambda ()
       (pcase kind
         ('followers (chirp-profile-open-followers handle))
         ('following (chirp-profile-open-following-users handle))
         (_ (user-error "Unknown profile list at point"))))
     :help-echo (pcase kind
                  ('followers "Open followers")
                  ('following "Open following")
                  (_ "Open profile list"))
     :properties `(chirp-profile-list-kind ,kind
                   chirp-profile-list-handle ,handle))))

(defun chirp-render--mark-profile-view-region (start end mode)
  "Mark the region from START to END as profile subview MODE."
  (chirp-render--add-action
   start end
   (lambda ()
     (unless (functionp chirp--profile-switch-mode-function)
       (user-error "This Chirp view cannot switch profile views"))
     (funcall chirp--profile-switch-mode-function mode))
   :help-echo "Switch profile view"
   :properties `(chirp-profile-view-mode ,mode)))

(defun chirp-render--mark-profile-action-region (start end action handle)
  "Mark the region from START to END as profile ACTION for HANDLE."
  (when handle
    (chirp-render--add-action
     start end
     (lambda ()
       (pcase action
         ('toggle-follow (chirp-toggle-follow-user-at-point))
         (_ (user-error "Unknown profile action at point"))))
     :help-echo "Toggle follow"
     :properties `(chirp-profile-action ,action
                   chirp-profile-action-handle ,handle))))

(defun chirp-render--profile-follow-action-label (user)
  "Return the primary follow button label for USER, or nil."
  (pcase-let (((map :self-p :viewer-following-p :viewer-followed-by-p) user))
    (cond
     (self-p nil)
     (viewer-following-p "Following")
     (viewer-followed-by-p "Follow back")
     (t "Follow"))))

(defun chirp-render-insert-profile-view-strip (current-mode modes)
  "Insert a lightweight profile subview strip for MODES.

CURRENT-MODE marks the active entry."
  (when modes
    (dolist (mode modes)
      (let ((start (point)))
        (insert
         (propertize
          (pcase mode
            ('posts "Posts")
            ('replies "Replies")
            ('highlights "Highlights")
            ('media "Media")
            ('likes "Likes")
            (_ (capitalize (symbol-name mode))))
          'face (if (eq mode current-mode)
                    'chirp-profile-view-active-face
                  'chirp-profile-view-inactive-face)))
        (chirp-render--mark-profile-view-region start (point) mode))
      (unless (eq mode (car (last modes)))
        (insert (propertize "  " 'face 'shadow))))
    (insert "\n\n")))

(defun chirp-render-insert-empty (message)
  "Insert MESSAGE for an empty state."
  (insert message)
  (insert "\n"))

;;; Text

(defun chirp-render--insert-prefix (prefix &optional face)
  "Insert PREFIX using FACE.

PREFIX may be an Appkit mutable prefix state; its first prefix is consumed."
  (when-let* ((text (if (appkit-ui-prefix-state-p prefix)
                        (appkit-ui-prefix-string prefix t)
                      prefix)))
    (insert (if face
                (propertize text 'face face)
              text))))

(defun chirp-render--prefix-string (prefix face)
  "Return PREFIX propertized with FACE, or nil.

An Appkit prefix state is read without consuming its current prefix."
  (when-let* ((text (if (appkit-ui-prefix-state-p prefix)
                        (appkit-ui-prefix-string prefix)
                      prefix)))
    (if face
        (propertize text 'face face)
      text)))

(defun chirp-render--apply-wrap-prefix (start end prefix face)
  "Apply PREFIX as the visual wrap prefix on text between START and END."
  (when (and prefix
             (< start end))
    (put-text-property start end
                       'wrap-prefix
                       (chirp-render--prefix-string prefix face))))

(defun chirp-render--metric-string (label value &optional active)
  "Return a metric string for LABEL and VALUE.

When ACTIVE is non-nil, emphasize the metric."
  (let* ((face (chirp-render--metric-face label active))
         (prefix
          (pcase label
            ('reply
             (if (fboundp 'nerd-icons-faicon)
                 (nerd-icons-faicon "nf-fa-reply" :face face)
               "Replies"))
            ('retweet
             (if (fboundp 'nerd-icons-faicon)
                 (nerd-icons-faicon "nf-fa-retweet" :face face)
               "RT"))
            ('like
             (if (fboundp 'nerd-icons-faicon)
                 (nerd-icons-faicon "nf-fa-heart" :face face)
               "Likes"))
            ('quote
             (if (fboundp 'nerd-icons-mdicon)
                 (nerd-icons-mdicon "nf-md-format_quote_open" :face face)
               "Quotes"))
            ('bookmark
             (if (fboundp 'nerd-icons-mdicon)
                 (nerd-icons-mdicon "nf-md-bookmark" :face face)
               "Bookmarks"))
            ('view
             (if (fboundp 'nerd-icons-mdicon)
                 (nerd-icons-mdicon "nf-md-eye" :face face)
               "Views"))
            (_
             (format "%s" label)))))
    (propertize (if (null value)
                    prefix
                  (format "%s %s" prefix (chirp-format-count value)))
                'face face)))

(cl-defun chirp-render--insert-metric (label value &key active action help-echo)
  "Insert the metric for LABEL and VALUE.

When ACTIVE is non-nil, emphasize the metric.  ACTION and HELP-ECHO
make the metric an Appkit action."
  (let ((start (point)))
    (insert (chirp-render--metric-string label value active))
    (when action
      (chirp-render--add-action
       start (point) action
       :help-echo help-echo))))

(defun chirp-render--reply-control-key (mode)
  "Return a comparison key for X reply-control MODE."
  (and (stringp mode)
       (replace-regexp-in-string "[_-]" "" (downcase mode))))

(defun chirp-render--reply-control-label (tweet)
  "Return the reply-control label for normalized TWEET, or nil."
  (pcase-let* (((map (:reply-control-mode mode) (:author-handle handle)
                     :reply-limited-p) tweet)
               (key (chirp-render--reply-control-key mode))
               (template (and key
                              (cdr (assoc-string
                                    key chirp-render--reply-control-labels t)))))
    (cond
     ((and template handle)
      (format template (concat "@" (string-remove-prefix "@" handle))))
     ((and mode
           (not (string-empty-p mode))
           (not (member key '("all" "everyone"))))
      "Only some accounts can reply.")
     (reply-limited-p
      "You cannot reply to this conversation"))))

(defun chirp-render--insert-reply-control
    (tweet &optional prefix prefix-face)
  "Insert the reply-control label for TWEET when one is available.

PREFIX and PREFIX-FACE control indentation."
  (when-let* ((label (chirp-render--reply-control-label tweet)))
    (insert (or (chirp-render--prefix-string prefix prefix-face) ""))
    (insert (propertize label 'face 'chirp-social-context-face))
    (insert "\n")))

(defun chirp-render--apply-text-entity (line-start text-offset line-len entity)
  "Apply one text ENTITY onto the inserted line at LINE-START.

TEXT-OFFSET is the character offset of this line in the original text.
LINE-LEN is the unpropertized line length.  ENTITY uses the character
offsets produced by `chirp--emit-visible-text'."
  (pcase-let (((map (:start beg) :end :kind :handle :tag :url) entity))
    (when (and (integerp beg)
               (integerp end)
               (< beg (+ text-offset line-len))
               (> end text-offset))
      (let ((from (+ line-start (max 0 (- beg text-offset))))
            (to (+ line-start (min line-len (- end text-offset)))))
        (when (< from to)
          (pcase kind
            ('mention
             (add-face-text-property from to 'chirp-link-face 'append)
             (chirp-render--add-profile-action
              from to handle))
            ('hashtag
             (chirp-render--add-action
              from to
              (lambda ()
                (chirp-timeline-open-search (concat "#" tag)))
              :help-echo (format "Search #%s" tag)
              :face 'chirp-hashtag-face))
            ('cashtag
             (chirp-render--add-action
              from to
              (lambda ()
                (chirp-timeline-open-search (concat "$" tag)))
              :help-echo (format "Search $%s" tag)
              :face 'chirp-hashtag-face))
            ((or 'url 'timestamp)
             (when url
               (add-face-text-property from to 'chirp-link-face 'append)
               (chirp-render--mark-url-region from to url)))))))))

(defun chirp-render--insert-filled-text (text &optional prefix prefix-face entities)
  "Insert TEXT and let Emacs wrap it visually in the current window.

Precede each line with PREFIX using PREFIX-FACE when provided.  ENTITIES
are the pre-parsed tweet-text spans from X, already mapped onto TEXT."
  (let* ((cleaned (chirp-clean-text text))
         (offset 0))
    (dolist (line (split-string cleaned "\n" nil))
      (chirp-render--insert-prefix prefix prefix-face)
      (let ((start (point))
            (line-len (length line)))
        (insert line)
        (dolist (entity entities)
          (chirp-render--apply-text-entity start offset line-len entity))
        (insert "\n")
        (chirp-render--apply-wrap-prefix start (point) prefix prefix-face)
        (setq offset (+ offset line-len 1))))))

(defun chirp-render--insert-face-text (text face &optional prefix prefix-face)
  "Insert TEXT using FACE, optionally preceded by PREFIX.

Apply PREFIX-FACE to that prefix when provided."
  (dolist (line (split-string (chirp-clean-text text) "\n" nil))
    (chirp-render--insert-prefix prefix prefix-face)
    (let ((start (point)))
      (if (string-empty-p line)
          (insert "")
        (insert (propertize line 'face face)))
      (insert "\n")
      (chirp-render--apply-wrap-prefix start (point) prefix prefix-face))))

(defun chirp-render--trailing-urls (tweet)
  "Return TWEET URLs that were not already inlined as text entities."
  (pcase-let (((map :text-entities :urls) tweet))
    (let ((inline (make-hash-table :test #'equal)))
      (dolist (entity text-entities)
        (pcase-let (((map :kind :url) entity))
          (when (and (eq kind 'url) (stringp url))
            (puthash url t inline))))
      (cl-remove-if (lambda (url)
                      (gethash url inline))
                    urls))))

(defun chirp-render--insert-expanded-urls (urls &optional prefix prefix-face)
  "Insert URLS as separate readable lines.

Precede each line with PREFIX using PREFIX-FACE when provided."
  (when urls
    (dolist (url urls)
      (chirp-render--insert-prefix prefix prefix-face)
      (let ((start (point)))
        (insert (propertize url 'face 'chirp-link-face))
        (let ((end (point)))
          (insert "\n")
          (chirp-render--apply-wrap-prefix start (point) prefix prefix-face)
          (chirp-render--mark-url-region start end url))))))

;;; Articles and Link Cards

(defun chirp-render--insert-article-preview (tweet &optional detailp prefix prefix-face)
  "Insert article metadata for TWEET.

When DETAILP is non-nil, use a longer preview.  Precede each line with PREFIX
using PREFIX-FACE when provided."
  (pcase-let* (((map (:article-title title) (:text raw-text)) tweet)
               (preview (chirp-tweet-article-preview tweet (if detailp 420 220)))
               (text (or raw-text "")))
    (when (and title
               (not (string-empty-p title))
               (not (string= title text)))
      (chirp-render--insert-face-text title 'chirp-article-title-face prefix prefix-face))
    (when (and preview
               (not (string-empty-p preview))
               (not (string= preview text))
               (not (string= preview title)))
      (chirp-render--insert-face-text preview 'chirp-article-summary-face prefix prefix-face))))

(defun chirp-render--insert-article-body (tweet &optional prefix prefix-face)
  "Insert the full article content for TWEET.

Precede each line with PREFIX using PREFIX-FACE when provided."
  (pcase-let* (((map (:article-title title) (:text raw-text) :article-text) tweet)
               (text (or raw-text "")))
    (when (and title
               (not (string-empty-p title))
               (not (string= title text)))
      (chirp-render--insert-face-text title 'chirp-article-title-face prefix prefix-face))
    (dolist (segment (chirp-article-segments article-text))
      (pcase-let (((map :type :text :media) segment))
        (pcase type
          ('text
           (chirp-render--insert-filled-text text prefix prefix-face)
           (insert "\n"))
          ('image
           (chirp-render-insert-media-strip (list media) prefix prefix-face)))))))

(defun chirp-render--insert-article-media-preview
    (tweet &optional detailp prefix prefix-face)
  "Insert article images for TWEET previews.

When DETAILP is non-nil, include every image.  Precede each line with PREFIX
using PREFIX-FACE when provided."
  (let ((images (chirp-tweet-article-images tweet (unless detailp 1))))
    (when images
      (chirp-render-insert-media-strip images prefix prefix-face))))

(defun chirp-render--article-expandable-p (tweet detailp)
  "Return non-nil when TWEET has article content beyond its preview.

DETAILP selects the same preview length used by the article renderer."
  (let* ((limit (if detailp 420 220))
         (segments (chirp-article-segments (plist-get tweet :article-text)))
         (text-count
          (cl-count-if (lambda (segment)
                         (eq (plist-get segment :type) 'text))
                       segments))
         (image-count
          (cl-count-if (lambda (segment)
                         (eq (plist-get segment :type) 'image))
                       segments))
         (preview (chirp-tweet-article-preview tweet limit))
         (full-summary
          (chirp-tweet-article-preview tweet most-positive-fixnum)))
    (and segments
         (or (> text-count 1)
             (> image-count 1)
             (and full-summary
                  (not (equal preview full-summary)))))))

(defun chirp-render--insert-show-more (tweet &optional prefix prefix-face)
  "Insert an inline expansion action for TWEET.

Precede the action with PREFIX using PREFIX-FACE when provided."
  (when-let* ((tweet-id (plist-get tweet :id)))
    (chirp-render--insert-prefix prefix prefix-face)
    (let ((start (point)))
      (insert (propertize "Show more" 'face 'link))
      (chirp-render--add-action
       start (point)
       (lambda ()
         (chirp--expand-tweet tweet-id))
       :help-echo "Show full content"
       :properties `(chirp-expand-tweet-id ,tweet-id))
      (insert "\n")
      (chirp-render--apply-wrap-prefix start (point) prefix prefix-face))))

(defun chirp-render--truncate-link-card-text (text max-length)
  "Return TEXT truncated to MAX-LENGTH characters when needed."
  (let ((cleaned (chirp-clean-text text)))
    (if (<= (length cleaned) max-length)
        cleaned
      (concat (string-trim-right (substring cleaned 0 (max 0 (- max-length 3))))
              "..."))))

(defun chirp-render--card-parent-prefix (prefix)
  "Return PREFIX as an Appkit card indent state, or nil."
  (cond
   ((appkit-ui-prefix-state-p prefix) prefix)
   ((and (stringp prefix) (not (string-empty-p prefix)))
    (appkit-ui-make-prefix-state prefix prefix))))

(cl-defun chirp-render--with-card (prefix inserter &key action help-echo properties)
  "Insert INSERTER's rows as one Appkit card nested below PREFIX.

INSERTER receives the card prefix state and should apply it to each
row, the same way disco and emacs-qq insert cards.  ACTION, HELP-ECHO,
and PROPERTIES apply to the finished card."
  (let* ((appkit-ui-card-indent-prefix-state
          (chirp-render--card-parent-prefix prefix))
         (card-prefix
          (appkit-ui-card-prefix-state
           :face 'chirp-quoted-tweet-border-face))
         (start (point)))
    (funcall inserter card-prefix)
    (when (< start (point))
      (when properties
        (add-text-properties start (point) properties))
      (when action
        (chirp-render--add-fallback-action
         start (max start (1- (point))) action
         :help-echo help-echo))
      (cons start (point)))))

(defun chirp-render--insert-link-card-image (url card-prefix)
  "Insert a sliced preview for cached card image URL using CARD-PREFIX."
  (when-let* ((image (and chirp-show-tweet-media
                          (chirp-media-cached-image
                           url
                           (* 2 chirp-media-thumbnail-size)
                           chirp-media-thumbnail-size))))
    (let ((start (point)))
      (appkit-media-insert-image-slices image nil nil "[link preview]")
      (insert "\n")
      (appkit-ui-apply-line-prefix start (point) card-prefix))))

(defun chirp-render--insert-link-card (card &optional prefix _prefix-face)
  "Insert one external link CARD as an Appkit card.

PREFIX supplies the card's outer nesting indentation."
  (pcase-let* (((map :url (:title raw-title) (:description raw-description)
                     (:domain raw-domain) :image-url) card)
               (title (chirp-first-nonblank raw-title))
               (description (chirp-first-nonblank raw-description))
               (domain (chirp-first-nonblank raw-domain)))
    (when (and description (equal description title))
      (setq description nil))
    (when (chirp-render--with-card
           prefix
           (lambda (card-prefix)
             (when domain
               (appkit-ui-insert-prefixed-lines
                card-prefix domain
                :face 'chirp-meta-face))
             (when title
               (appkit-ui-insert-prefixed-lines
                card-prefix
                (chirp-render--truncate-link-card-text title 180)
                :face 'chirp-link-card-title-face))
             (when description
               (appkit-ui-insert-prefixed-lines
                card-prefix
                (chirp-render--truncate-link-card-text description 220)
                :face 'chirp-link-card-description-face))
             (chirp-render--insert-link-card-image image-url card-prefix))
           :action (and (stringp url)
                        (not (string-empty-p url))
                        (lambda ()
                          (browse-url url)))
           :help-echo (and url (format "Open %s" url))
           :properties `(chirp-subentry-url ,url))
      (insert "\n"))))

(defun chirp-render--insert-link-cards (tweet &optional prefix prefix-face)
  "Insert website card previews for TWEET.

Precede each card with PREFIX using PREFIX-FACE when provided."
  (dolist (card (chirp-media-link-cards-for-tweet tweet))
    (chirp-render--insert-link-card card prefix prefix-face)))

;;; Polls

(defun chirp-render--poll-status (poll state)
  "Return the visible status of POLL, including pending vote STATE.
Only the server's final-counts flag identifies final results."
  (cond
   ((eq state 'unknown)
    "Vote outcome unknown; refresh to check X")
   (state "Submitting vote")
   ((plist-get poll :counts-final-p) "Final results")
   ((chirp-poll-closed-p poll) "Voting closed; results not final")
   ((numberp (plist-get poll :end-time))
    (let ((seconds (max 1 (ceiling (- (plist-get poll :end-time)
                                    (float-time))))))
      (cond
       ((>= seconds 86400)
        (format "%dd %dh left" (/ seconds 86400) (/ (% seconds 86400) 3600)))
       ((>= seconds 3600)
        (format "%dh %dm left" (/ seconds 3600) (/ (% seconds 3600) 60)))
       ((>= seconds 60) (format "%dm left" (/ seconds 60)))
       (t "Less than a minute left"))))
   (t "Closing time unavailable")))

(defun chirp-render--insert-poll (tweet prefix write-actions-p)
  "Insert TWEET's poll as a card nested below PREFIX.
WRITE-ACTIONS-P permits voting only in an open, unvoted poll.  Results
remain hidden until the viewer has voted or voting has closed."
  (when-let* ((poll (plist-get tweet :poll))
              (options (plist-get poll :options)))
    (let* ((selected (plist-get poll :selected-choice))
           (results-p (or selected (chirp-poll-closed-p poll)))
           (state (and (fboundp 'chirp-poll-pending-p)
                       (chirp-poll-pending-p (plist-get tweet :id))))
           (votable-p (and write-actions-p
                           (not state)
                           (chirp-poll-votable-p poll)))
           (total (plist-get poll :total-votes))
           (maximum (apply #'max (mapcar (lambda (option)
                                          (plist-get option :count))
                                        options))))
      (chirp-render--with-card
       prefix
       (lambda (card-prefix)
         (let ((bar-width
                (max 4 (min 20 (- (chirp--view-width)
                                  (string-width
                                   (appkit-ui-prefix-string card-prefix))
                                  12)))))
           (dolist (option options)
             (let* ((id (plist-get option :id))
                    (count (plist-get option :count))
                    (selected-p (equal selected id))
                    (winner-p (and results-p (> maximum 0)
                                   (= count maximum)))
                    (faces (append
                            (when selected-p '(chirp-poll-selected-face))
                            (when winner-p '(chirp-poll-winner-face))))
                    (label (concat (if selected-p "[x] " "[ ] ")
                                   (chirp-clean-text
                                    (plist-get option :label))))
                    (start (point)))
               (if votable-p
                   (appkit-ui-insert-action-button
                    label
                    (lambda ()
                      ;; A previous click may still be awaiting a redraw.
                      (when (and (not (chirp-poll-pending-p
                                       (plist-get tweet :id)))
                                 (chirp-poll-votable-p poll))
                        (chirp-poll-vote tweet id)))
                    :face 'chirp-poll-option-face
                    :help-echo "Vote for this choice (votes cannot be changed)")
                 (insert (if faces (propertize label 'face faces) label)))
               (when results-p
                 (let ((ratio (if (> total 0) (/ (float count) total) 0.0)))
                   (insert
                    (propertize
                     (format "  %.1f%%  [%s]"
                             (* 100 ratio)
                             (appkit-ui-progress-bar ratio bar-width ?= ?.))
                     'face (or faces 'chirp-meta-face)))))
               (insert "\n")
               (appkit-ui-apply-line-prefix start (point) card-prefix)))
           (appkit-ui-insert-prefixed-lines
            card-prefix
            (format "%d %s · %s"
                    total (if (= total 1) "vote" "votes")
                    (chirp-render--poll-status poll state))
            :face 'chirp-meta-face))))
      (insert "\n"))))

;;; Quoted Tweets and Replies

(defvar chirp-render--quoted-tweet-depth 0
  "Dynamic nesting depth while rendering quoted tweets.")

(cl-defun chirp-render-insert-tweet-card
    (tweet &key prefix write-actions-p)
  "Insert normalized TWEET as a nested card with ordinary body text.

PREFIX supplies outer indentation.  WRITE-ACTIONS-P controls mutation actions.
Only local links and controls are mouse actions; the body remains scrollable."
  (chirp-render--with-card
   prefix
   (lambda (card-prefix)
     (let ((body-start (point)))
       (chirp-render--insert-tweet
        tweet
        :show-reply-context t
        :write-actions-p write-actions-p)
       (appkit-ui-apply-line-prefix body-start (point) card-prefix)))
   :properties
   `(chirp-subentry-item ,tweet
     chirp-subentry-url ,(plist-get tweet :url))))

(defun chirp-render--insert-quoted-tweet
    (tweet &optional prefix prefix-face write-actions-p)
  "Insert a normal tweet presentation inside TWEET's card.

PREFIX supplies the card's outer nesting indentation.  PREFIX-FACE is accepted
for caller consistency; the card owns its border face.  WRITE-ACTIONS-P
controls mutation actions.  Nested quoted tweets are omitted after one level."
  (ignore prefix-face)
  (when-let* ((quoted (plist-get tweet :quoted-tweet))
              ((not (plist-get quoted :deleted-p)))
              ((< chirp-render--quoted-tweet-depth 1))
              (span
               (let ((chirp-render--quoted-tweet-depth
                      (1+ chirp-render--quoted-tweet-depth)))
                 (chirp-render-insert-tweet-card
                  quoted
                  :prefix prefix
                  :write-actions-p write-actions-p))))
    (put-text-property (car span) (1+ (car span))
                       'chirp-entry-start nil)))

(defun chirp-render--insert-list-reply-context (tweet reply-parent &optional prefix prefix-face)
  "Insert a lightweight reply context line for TWEET above REPLY-PARENT.

Precede the line with PREFIX using PREFIX-FACE when provided."
  (let ((parent-id (plist-get reply-parent :id))
        (handle (or (plist-get tweet :reply-to-handle)
                    (plist-get reply-parent :author-handle))))
    (when parent-id
      (chirp-render--insert-prefix prefix prefix-face)
      (let ((start (point))
            handle-start handle-end)
        (if handle
            (progn
              (insert (propertize "↳ replying to "
                                  'face 'chirp-thread-reply-context-face))
              (setq handle-start (point))
              (insert (propertize (format "@%s" handle)
                                  'face 'chirp-link-face))
              (setq handle-end (point))
              (insert (propertize " above"
                                  'face 'chirp-thread-reply-context-face)))
          (insert (propertize "↳ reply to above"
                              'face 'chirp-thread-reply-context-face)))
        (insert "\n")
        (chirp-render--apply-wrap-prefix start (point) prefix prefix-face)
        (chirp-render--add-action
         start (max start (1- (point)))
         #'chirp-open-reply-parent-at-point
         :help-echo "Open parent tweet"
         :properties `(chirp-reply-parent-id ,parent-id))
        (when handle-start
          (chirp-render--add-profile-action handle-start handle-end handle))))))

(defun chirp-render--list-reply-parent (tweet previous)
  "Return PREVIOUS when TWEET looks like a reply to it."
  (when previous
    (pcase-let (((map :reply-to-id :reply-to-handle :conversation-id) tweet)
                ((map (:id previous-id) (:author-handle previous-handle)
                      (:conversation-id previous-conversation-id)) previous))
      (when (or (and reply-to-id
                     previous-id
                     (equal reply-to-id previous-id))
                (and reply-to-handle
                     previous-handle
                     (equal reply-to-handle previous-handle)
                     conversation-id
                     (or (equal conversation-id previous-id)
                         (equal conversation-id previous-conversation-id))))
        previous))))

;;; Media

(defun chirp-render--insert-avatar (url &optional handle)
  "Insert an avatar for URL when possible.

Associate the avatar with HANDLE when provided."
  (when chirp-show-avatars
    (let ((start (point)))
      (insert (appkit-chat-avatar-one-line-string
               (chirp-media-avatar-image url) "  "))
      (chirp-render--add-profile-action start (point) handle))))

(defun chirp-render--thumbnail-width (rows)
  "Return the reserved column width in characters for slice ROWS."
  (let* ((display (and rows (get-text-property 0 'display (car rows))))
         (image (and (eq (car-safe (car-safe display)) 'slice)
                     (cadr display))))
    (max 1 (ceiling
            (or (car (ignore-errors (image-size image)))
                1)))))

(defun chirp-render--media-placeholder-text (media &optional compactp)
  "Return a text placeholder for MEDIA.

When COMPACTP is non-nil, omit alt text and make a missing video actionable."
  (pcase-let* (((map (:alt raw-alt) :type :width :height) media)
               (alt (chirp-first-nonblank raw-alt))
               (kind (pcase type
                 ("video"
                  (format "video%s"
                          (if (and width height)
                              (format " %sx%s" width height)
                            (if compactp " open" ""))))
                 ("animated_gif" "gif")
                 (_ "image"))))
    (if (and (not compactp)
             alt
             (stringp alt)
             (not (string-empty-p alt)))
        (format "[%s: %s]" kind (chirp-render--truncate-link-card-text alt 220))
      (format "[%s]" kind))))

(defun chirp-render--mark-media-region (start end media media-list index)
  "Mark START..END as MEDIA at INDEX in MEDIA-LIST."
  (chirp-render--add-action
   start end
   (lambda ()
     (chirp-media-open media-list index
                       (or chirp--view-title "Chirp Media")))
   :help-echo "Open media"
   :properties `(chirp-media-item ,media
                 chirp-media-index ,index
                 chirp-media-list ,media-list)))

(defun chirp-render--media-cell (media index image)
  "Return sliced cell data for MEDIA at INDEX using IMAGE."
  (if-let* ((display-image
             (or image
                 (chirp-media-thumbnail-placeholder-image media)))
            (rows (appkit-media-image-slice-rows display-image)))
      (list :media media
            :index index
            :rows rows
            :padding (propertize
                      " " 'display
                      `(space :width ,(chirp-render--thumbnail-width rows))))
    (let ((placeholder (chirp-render--media-placeholder-text media t)))
      (list :media media
            :index index
            :rows (list (propertize placeholder
                                    'face 'chirp-media-placeholder-face))
            :padding (make-string (max 1 (string-width placeholder)) ?\s)))))

(defun chirp-render--insert-media-cell-slice
    (cell row media-list)
  "Insert ROW of CELL and associate it with MEDIA-LIST."
  (let ((start (point)))
    (pcase-let (((map :rows :padding :media :index) cell))
      (insert (or (nth row rows) padding))
      (chirp-render--mark-media-region
       start (point) media media-list index))))

(defun chirp-render--insert-media-row
    (cells media-list gap prefix prefix-face)
  "Insert CELLS as one sliced row associated with MEDIA-LIST.

GAP is the pixel gutter.  PREFIX and PREFIX-FACE control indentation."
  (let ((row-count
         (apply #'max
                (mapcar (lambda (cell)
                          (length (plist-get cell :rows)))
                        cells))))
    (dotimes (row row-count)
      (chirp-render--insert-prefix prefix prefix-face)
      (cl-loop for cell in cells
               for column from 0
               do (when (> column 0)
                    (insert
                     (propertize " " 'display
                                 `(space :width (,gap)))))
               do (chirp-render--insert-media-cell-slice
                   cell row media-list))
      (when (< (1+ row) row-count)
        (insert (propertize "\n" 'line-height t))))))

(defun chirp-render--media-track-offsets (image gap)
  "Return IMAGE item offsets in SVG pixels, separated by GAP."
  (let ((offset 0))
    (mapcar
     (lambda (width)
       (prog1 offset
         (setq offset (+ offset width gap))))
     (plist-get (cdr image) :appkit-media-strip-widths))))

(defun chirp-render--media-track-reset-hscroll ()
  "Reset horizontal scrolling after point leaves a focused media track."
  (unless (get-text-property (point) 'chirp-media-track)
    (set-window-hscroll (selected-window) 0)))

(defun chirp-render--media-track-select (state delta)
  "Move media track STATE by DELTA items and reveal the selected item."
  (when-let* ((inline (aref state 9)))
    (appkit-media-video-inline-close inline)
    (aset state 9 nil)
    (aset state 12 nil))
  (let* ((media-list (aref state 2))
         (count (length media-list))
         (index (mod (+ (aref state 0) delta) count))
         (offset (nth index (aref state 1))))
    (if-let* ((image
               (chirp-media-carousel-image
                media-list
                (aref state 6)
                (aref state 5)
                offset
                (aref state 7)
                (aref state 8)))
              (rows (appkit-media-image-slice-rows image))
              (same-size (= (length rows) (length (aref state 4)))))
        (progn
          (let ((inhibit-read-only t))
            (cl-mapc
             (lambda (position row)
               (put-text-property
                position (1+ position) 'display
                (get-text-property 0 'display row)))
             (aref state 4) rows))
          (aset state 0 index)
          (aset state 10 image)
          (when (eq (window-buffer (selected-window))
                    (current-buffer))
            (set-window-hscroll (selected-window) 0))
          (message
           "Media %d of %d; %s"
           (1+ index) count
           (if (chirp-media-video-like-p (nth index media-list))
               "RET plays it; m toggles audio"
             "RET opens it")))
      (user-error "Unable to reveal media item %d" (1+ index)))))

(defun chirp-render--media-track-open (state)
  "Open or toggle the currently selected item in media track STATE."
  (let ((media (nth (aref state 0) (aref state 2))))
    (if (and chirp-video-use-internal-player
             (chirp-media-video-like-p media))
        (chirp-render--media-track-toggle-video state)
      (when-let* ((inline (aref state 9)))
        (appkit-media-video-inline-close inline)
        (aset state 9 nil)
        (aset state 12 nil))
      (chirp-media-open
       (aref state 2) (aref state 0) (aref state 3)))))

(defun chirp-render--media-track-toggle-muted (state)
  "Toggle canonical audio mute state for media track STATE."
  (let* ((inline (chirp-render--media-track-inline state))
         (muted
          (if inline
              (not (appkit-media-video-inline-muted-p inline))
            (not (aref state 13)))))
    (aset state 13 muted)
    (when inline
      (appkit-media-video-inline-set-muted inline muted))
    (message "Video audio %s" (if muted "muted" "unmuted"))))

(defun chirp-render--media-track-state (&optional event)
  "Return the media track state at point or mouse EVENT."
  (let* ((position (and event (posn-point (event-start event))))
         (window (and event (posn-window (event-start event))))
         (buffer (and (windowp window) (window-buffer window)))
         (position
          (and (integer-or-marker-p position)
               (if (markerp position) (marker-position position) position))))
    (or (and position
             (buffer-live-p buffer)
             (with-current-buffer buffer
               (and (<= (point-min) position)
                    (< position (point-max))
                    (get-text-property position 'chirp-media-track-state))))
        (get-char-property (point) 'chirp-media-track-state)
        (user-error "No media track at point"))))

(defun chirp-render--media-hotspot-index ()
  "Return the media item index encoded in the current key sequence."
  (or
   (cl-loop for event across (this-command-keys-vector)
            for name = (and (symbolp event) (symbol-name event))
            when (and name
                      (string-match
                       "\\`chirp-media-\\([0-9]+\\)\\'" name))
            return (string-to-number (match-string 1 name)))
   (user-error "No media item in the current event")))

(defun chirp-render-media-track-next ()
  "Select the next item in the media track at point."
  (interactive)
  (chirp-render--media-track-select (chirp-render--media-track-state) 1))

(defun chirp-render-media-track-previous ()
  "Select the previous item in the media track at point."
  (interactive)
  (chirp-render--media-track-select (chirp-render--media-track-state) -1))

(defun chirp-render-media-track-open ()
  "Activate the selected item in the media track at point."
  (interactive)
  (chirp-render--media-track-open (chirp-render--media-track-state)))

(defun chirp-render-media-track-open-dedicated ()
  "Open the selected track item in the dedicated media viewer."
  (interactive)
  (let* ((state (chirp-render--media-track-state))
         (inline
           (and (equal (aref state 12) (aref state 0))
                (aref state 9))))
    (chirp-media-open-dedicated
     (chirp-media-selection-create
      (aref state 2) (aref state 0) inline)
     (aref state 3))))

(defun chirp-render-media-track-open-external ()
  "Open the selected track video in the configured external player."
  (interactive)
  (let ((state (chirp-render--media-track-state)))
    (chirp-media-play-video
     (nth (aref state 0) (aref state 2)) t)))

(defun chirp-render-media-track-toggle-muted ()
  "Toggle audio for the media track at point."
  (interactive)
  (chirp-render--media-track-toggle-muted
   (chirp-render--media-track-state)))

(defun chirp-render-media-track-open-hotspot (event)
  "Activate the media item identified by an `image-map' EVENT."
  (interactive "e")
  (let ((state (chirp-render--media-track-state event)))
    (aset state 0 (chirp-render--media-hotspot-index))
    (chirp-render--media-track-open state)))

(defun chirp-render--media-track-hotspot-map
    (position media-list image height gap widths fit)
  "Return keyboard and image-map-style actions for a media track.

POSITION supplies its existing keymap.  MEDIA-LIST and IMAGE identify the
track; HEIGHT, GAP, WIDTHS, and FIT retain its presentation geometry."
  (let* ((map
          (copy-keymap
           (or (get-text-property position 'keymap)
               (make-sparse-keymap))))
         (parent (keymap-parent map))
         (state
          (vector
           0
           (chirp-render--media-track-offsets image gap)
           media-list
           (or chirp--view-title "Chirp Media")
           nil
           gap
           height
           widths
           fit
           nil
           image
           nil
           nil
           nil
           map)))
    (set-keymap-parent
     map (if parent
             (make-composed-keymap (list video-inline-map parent))
           video-inline-map))
    (define-key map [remap video-inline-toggle]
                #'chirp-render-media-track-toggle-video)
    (define-key map [remap video-inline-loop]
                #'chirp-render-media-track-loop)
    (define-key map [remap video-inline-mute]
                #'chirp-render-media-track-toggle-muted)
    (define-key map [remap video-inline-frame]
                #'chirp-render-media-track-frame)
    (define-key map [remap video-inline-view]
                #'chirp-render-media-track-open-dedicated)
    (define-key map [right] #'chirp-render-media-track-next)
    (define-key map [left] #'chirp-render-media-track-previous)
    (define-key map (kbd "RET") #'chirp-render-media-track-open)
    (define-key map (kbd "<return>") #'chirp-render-media-track-open)
    (define-key map (kbd "S-RET")
                #'chirp-render-media-track-open-external)
    (define-key map (kbd "M-RET") #'chirp-open-entry-at-point)
    (define-key map (kbd "m") #'chirp-render-media-track-toggle-muted)
    (cl-loop for _media in media-list
             for index from 0
             for id = (intern (format "chirp-media-%d" index))
             do
             (define-key map (vector id 'down-mouse-1) #'ignore)
             (define-key map (vector id 'mouse-1)
                         #'chirp-render-media-track-open-hotspot))
    (cons map state)))

(defun chirp-render--media-track-markers-visible-p (buffer markers)
  "Return non-nil when one of MARKERS is visible in BUFFER."
  (and (buffer-live-p buffer)
       (cl-some
        (lambda (window)
          (cl-some
           (lambda (marker)
             (and (marker-position marker)
                  (pos-visible-in-window-p marker window t)))
           markers))
        (get-buffer-window-list buffer nil t))))

(defun chirp-render--media-track-activate-video
    (buffer markers poster canvas)
  "Replace POSTER rows at MARKERS in BUFFER with video CANVAS slices."
  (when-let* ((slice-count
               (plist-get (cdr poster) :appkit-media-nslices)))
    (plist-put (cdr canvas) :appkit-media-nslices slice-count))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((rows (appkit-media-image-slice-rows canvas)))
        (unless (= (length rows) (length markers))
          (error "Video Canvas changed media track slice geometry"))
        (let ((inhibit-read-only t))
          (cl-mapc
           (lambda (marker row)
             (when (and (marker-position marker)
                        (eq (marker-buffer marker) buffer))
               (put-text-property
                marker (1+ marker) 'display
                (get-text-property 0 'display row))))
           markers rows))))))

(defun chirp-render--media-track-prepare-video-host (state)
  "Attach backend-neutral dynamic scene state to media track STATE."
  (when (cl-some #'chirp-media-video-like-p (aref state 2))
    (let ((token (list 'chirp-inline-video)))
      (dolist (marker (aref state 4))
        (put-text-property marker (1+ marker)
                           'chirp-video-inline-token token))
      (aset state 11 token)))
  state)

(defun chirp-render--media-track-scene-plan (state)
  "Return scene geometry for STATE's currently displayed viewport."
  (chirp-media-carousel-plan
   (aref state 2)
   (aref state 6)
   (aref state 5)
   (or (plist-get (cdr (aref state 10))
                  :appkit-media-strip-offset)
       0)
   (aref state 7)
   (aref state 8)))

(defun chirp-render--media-track-scene-width (state)
  "Return the complete media scene width for STATE."
  (plist-get (chirp-render--media-track-scene-plan state) :width))

(defun chirp-render--media-track-scene-canvas (state &optional plan)
  "Build a static Canvas scene for media track STATE using optional PLAN."
  (setq plan (or plan (chirp-render--media-track-scene-plan state)))
  (pcase-let* (((map :width :height :offset :items) plan)
               (poster (aref state 10))
               (background (video-background-color nil (car (aref state 4))))
               (canvas (video-canvas-create width height background)))
    (dolist (cell items)
      (pcase-let* (((map :item :x (:width cell-width)) cell)
                   ((map :file :fit) item))
        (when file
          (video-canvas-draw-source
           canvas width height file (- x offset) 0 cell-width height
           :fit (or fit 'contain) :background background))))
    (dolist (property '(:appkit-media-nslices :map
                        :appkit-media-strip-widths
                        :appkit-media-strip-offset))
      (when-let* ((value (plist-get (cdr poster) property)))
        (plist-put (cdr canvas) property value)))
    (canvas-refresh canvas)
    canvas))

(defun chirp-render--media-track-inline (state)
  "Return STATE's live inline for its selected video item, if any."
  (let ((inline (aref state 9)))
    (and (chirp-media-video-like-p (nth (aref state 0) (aref state 2)))
         (equal (aref state 12) (aref state 0))
         (appkit-media-video-inline-p inline)
         (not (appkit-media-video-inline-closed-p inline))
         inline)))

(defun chirp-render--media-track-ensure-video (state)
  "Return or create STATE's selected live inline without starting playback."
  (let* ((index (aref state 0))
         (media (nth index (aref state 2))))
    (unless (chirp-media-video-like-p media)
      (user-error "Current media is not a video"))
    (or (chirp-render--media-track-inline state)
        (progn
          (when-let* ((inline (aref state 9)))
            (appkit-media-video-inline-close inline))
          (pcase-let* ((buffer (current-buffer))
                       (markers (aref state 4))
                       (token (aref state 11))
                       (poster (aref state 10))
                       (plan (chirp-render--media-track-scene-plan state))
                       ((map :items (:width scene-width) :offset) plan)
                       (selected-cell (nth index items))
                       ((map :x (:width target-width)) selected-cell)
                       (scene (chirp-render--media-track-scene-canvas state plan))
                       (target-x (- x offset))
                 (session
                  (or (chirp-media-video-session-create
                       media (aref state 13))
                      (user-error "Current media has no playable URL")))
                 (inline
                   (appkit-media-video-inline-create
                    session target-width (aref state 6)
                    :poster poster
                    :fit (or (aref state 8) 'contain)
                    :canvas scene
                    :canvas-width scene-width
                    :canvas-height (aref state 6)
                    :destination-x target-x
                    :destination-y 0
                    :anchor (car markers)
                    :visible-function
                    (lambda (_inline)
                      (chirp-render--media-track-markers-visible-p buffer markers))
                    :alive-function
                    (lambda (_inline)
                      (and (buffer-live-p buffer)
                           (marker-position (car markers))
                           (eq (get-text-property
                                (car markers) 'chirp-video-inline-token)
                               token)))
                    :activate-function
                    (lambda (_inline canvas)
                      (chirp-render--media-track-activate-video
                       buffer markers poster canvas))
                    :close-function
                    (lambda (closed-inline)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (chirp-media-unregister-video-inline closed-inline)
                          (when (eq (aref state 9) closed-inline)
                            (aset state 9 nil)
                            (aset state 12 nil))))))))
            (aset state 9 inline)
            (aset state 12 index)
            (chirp-media-register-video-inline (aref state 2) index inline)
            (appkit-media-video-inline-bind-controls inline (aref state 14))
            inline)))))

(defun chirp-render-media-track-toggle-video ()
  "Toggle inline playback for the selected video in the track at point."
  (interactive)
  (chirp-render--media-track-toggle-video (chirp-render--media-track-state)))

(defun chirp-render-media-track-loop ()
  "Toggle looping for the selected initialized video in the track at point."
  (interactive)
  (let* ((state (chirp-render--media-track-state))
         (inline (chirp-render--media-track-inline state)))
    (unless inline
      (user-error "Current media has no initialized inline video"))
    (appkit-media-video-inline-toggle-loop inline)))

(defun chirp-render-media-track-frame ()
  "Present the selected video in another frame without changing playback."
  (interactive)
  (let* ((state (chirp-render--media-track-state))
         (inline (chirp-render--media-track-ensure-video state)))
    (appkit-media-present-video-inline
     inline (aref state 3)
     :display-function #'video-display-buffer-other-frame)))

(defun chirp-render--media-track-toggle-video (state)
  "Toggle inline playback for STATE's selected video item."
  (if-let* ((inline (chirp-render--media-track-inline state)))
      (appkit-media-video-inline-toggle inline)
    (appkit-media-video-inline-play
     (chirp-render--media-track-ensure-video state))))

(defun chirp-render--insert-media-track
    (media-list prefix prefix-face height gap widths fit)
  "Insert MEDIA-LIST as one unbreakable horizontal track.

PREFIX and PREFIX-FACE control indentation.  HEIGHT, GAP, WIDTHS, and FIT
describe the shared carousel geometry."
  (if-let* ((image
             (chirp-media-carousel-image
              media-list height gap nil widths fit))
            (rows (appkit-media-image-slice-rows image)))
      (let (hotspot-map track-state track-positions)
        (setq-local auto-hscroll-mode nil)
        (add-hook 'post-command-hook
                  #'chirp-render--media-track-reset-hscroll nil t)
        (cl-loop for row in rows
                 for row-index from 0
                 do
                 (unless (zerop row-index)
                   (insert (propertize "\n" 'line-height t)))
                 (chirp-render--insert-prefix prefix prefix-face)
                 (let ((start (point)))
                   (insert row)
                   (push start track-positions)
                   (chirp-render--mark-media-region
                    start (point) (car media-list) media-list 0)
                   (unless hotspot-map
                     (pcase-let
                         ((`(,map . ,state)
                           (chirp-render--media-track-hotspot-map
                            start media-list image
                            height gap widths fit)))
                       (setq hotspot-map map
                             track-state state)))
                   (put-text-property start (point) 'keymap hotspot-map)
                   (put-text-property start (point)
                                      'chirp-media-track t)))
        (aset track-state 4
              (mapcar #'copy-marker (nreverse track-positions)))
        (dolist (marker (aref track-state 4))
          (put-text-property marker (1+ marker)
                             'chirp-media-track-state track-state))
        (chirp-render--media-track-prepare-video-host track-state))
    (chirp-render--insert-media-grid media-list prefix prefix-face)))

(defun chirp-render--media-aspect-ratio (media)
  "Return MEDIA's positive natural aspect ratio, or nil."
  (pcase-let (((map :width :height) media))
    (when (and (numberp width) (> width 0)
               (numberp height) (> height 0))
      (/ (float width) height))))

(defun chirp-render--insert-media-carousel
    (media-list prefix prefix-face)
  "Insert MEDIA-LIST with PREFIX and PREFIX-FACE using X's presentation."
  (pcase-let* ((plan
                (chirp-media-layout-carousel-plan
                 (mapcar #'chirp-render--media-aspect-ratio media-list)
                 (* 2 chirp-media-thumbnail-size)))
               ((map :height :widths :fit) plan))
    (if (and height widths)
        (chirp-render--insert-media-track
         media-list prefix prefix-face
         height chirp-media-layout-carousel-gap widths
         fit)
      (chirp-render--insert-media-grid
       media-list prefix prefix-face))))

(defun chirp-render--insert-media-grid
    (media-list prefix prefix-face)
  "Insert MEDIA-LIST using X Web's compact cover grid.

PREFIX and PREFIX-FACE control indentation."
  (let ((count (min (length media-list) 6)))
    (if (= count 1)
        (chirp-render--insert-media-row
         (list
          (chirp-render--media-cell
           (car media-list) 0
           (chirp-media-thumbnail-image (car media-list))))
         media-list 0 prefix prefix-face)
      (pcase-let* ((plan
                    (chirp-media-layout-cover-plan
                     count chirp-media-thumbnail-size (frame-char-height)))
                   ((map :bands :band-slices :crop-specs) plan)
                   (cells (make-vector count nil))
                   (offsets (make-hash-table :test #'eql))
                   (prepared-bands nil))
        (cl-loop for media in media-list
                 for index below count
                 do (aset
                     cells index
                     (chirp-render--media-cell
                      media index
                      (chirp-media-thumbnail-image
                       media (aref crop-specs index)))))
        (setq prepared-bands
              (mapcar
               (lambda (band)
                 (prog1
                     (mapcar
                      (lambda (index)
                        (cons index (gethash index offsets 0)))
                      band)
                   (dolist (index band)
                     (puthash index
                              (+ (gethash index offsets 0) band-slices)
                              offsets))))
               bands))
        (cl-loop for band in prepared-bands
                 for band-index from 0
                 do
                 (dotimes (row band-slices)
                   (chirp-render--insert-prefix prefix prefix-face)
                   (cl-loop for (index . offset) in band
                            for column from 0
                            do (when (> column 0)
                                 (insert
                                  (propertize
                                   " " 'display
                                   `(space :width
                                     (,chirp-media-layout-cover-gap)))))
                            do (chirp-render--insert-media-cell-slice
                                (aref cells index)
                                (+ offset row)
                                media-list))
                   (unless (and (= band-index (1- (length prepared-bands)))
                                (= row (1- band-slices)))
                     (insert (propertize "\n" 'line-height t)))))))))

(defun chirp-render--insert-media-text-cell (media media-list index &optional prefix prefix-face)
  "Insert one compact text entry for hidden MEDIA.

Associate it with INDEX in MEDIA-LIST.  Precede it with PREFIX using
PREFIX-FACE when provided."
  (chirp-render--insert-prefix prefix prefix-face)
  (let ((start (point)))
    (insert (propertize (chirp-render--media-placeholder-text media)
                        'face 'chirp-media-placeholder-face))
    (chirp-render--mark-media-region start (point) media media-list index)))

(defun chirp-render-insert-media-strip
    (media-list &optional prefix prefix-face presentation)
  "Insert MEDIA-LIST using a carousel or compact cover grid.

Precede each row with PREFIX using PREFIX-FACE when provided.  PRESENTATION is
`carousel' for X Web's current non-condensed multi-item layout; compact
contexts use the cover grid."
  (when media-list
    (if (not chirp-show-tweet-media)
        (cl-loop for media in media-list
                 for index from 0
                 do (unless (zerop index)
                      (insert "\n"))
                 do (chirp-render--insert-media-text-cell media media-list index prefix prefix-face))
      (if (eq presentation 'carousel)
          (chirp-render--insert-media-carousel media-list prefix prefix-face)
        (chirp-render--insert-media-grid media-list prefix prefix-face)))
    (insert "\n\n")))

;;; Tweets

(cl-defun chirp-render--insert-tweet-heading
    (tweet &key prefix prefix-face avatar-p (time-p t) (time-format 'compact)
           (newline-p t))
  "Insert TWEET's author heading.

PREFIX and PREFIX-FACE control indentation.  AVATAR-P controls the avatar;
TIME-P controls the timestamp, and TIME-FORMAT is `compact' or `full'.
NEWLINE-P controls the trailing newline."
  (pcase-let* (((map :author-name (:author-handle handle) :created-at
                     :author-avatar-url) tweet)
               (author (or author-name "Unknown")))
    (chirp-render--insert-prefix prefix prefix-face)
    (when avatar-p
      (chirp-render--insert-avatar
       author-avatar-url handle))
    (let ((author-start (point)))
      (insert (propertize author 'face 'chirp-author-face))
      (when handle
        (insert " ")
        (insert (propertize (format "@%s" handle) 'face 'chirp-handle-face)))
      (chirp-render--add-profile-action author-start (point) handle))
    (when (and time-p created-at)
      (when-let* ((time
                   (pcase time-format
                     ('compact (chirp-time-format-compact created-at))
                     ('full (chirp-time-format-full created-at))
                     (_ (error "Invalid Chirp tweet time format: %S"
                               time-format))))
                  ((not (string-empty-p time))))
        (appkit-chat-ins-insert-right-aligned-text
         time (chirp--view-width)
         :face 'chirp-meta-face
         :right-edge-margin 0)))
    (when newline-p
      (insert "\n"))))

(defun chirp-render--insert-edit-history-context
    (tweet &optional prefix prefix-face)
  "Insert TWEET's edit-history action.

PREFIX and PREFIX-FACE control indentation."
  (pcase-let (((map :edited-p :edit-history-ids) tweet))
    (when edited-p
      (let ((count (length edit-history-ids))
            (start (point)))
        (chirp-render--insert-prefix prefix prefix-face)
        (insert (format "Edited · %d version%s"
                        count
                        (if (= count 1) "" "s")))
        (chirp-render--add-action
         start (point)
         (lambda () (chirp-edit-history-open-tweet tweet))
         :help-echo "Open edit history"
         :face 'chirp-social-context-face)
        (insert "\n")))))

(cl-defun chirp-render--insert-tweet-context
    (tweet &key prefix prefix-face reply-parent (trailing-newline-p t))
  "Insert social and parent context for TWEET.

PREFIX and PREFIX-FACE control indentation.  REPLY-PARENT supplies the
preceding tweet.  TRAILING-NEWLINE-P controls whether a non-empty context
block ends in a newline."
  (let ((start (point)))
    (when reply-parent
      (chirp-render--insert-list-reply-context
       tweet reply-parent prefix prefix-face))
    (chirp-render--insert-edit-history-context tweet prefix prefix-face)
    (pcase (plist-get tweet :timeline-context)
      ('related
       (chirp-render--insert-prefix prefix prefix-face)
       (insert (propertize "Related tweet"
                           'face 'chirp-thread-related-context))
       (insert "\n"))
      ('pinned
       (chirp-render--insert-prefix prefix prefix-face)
       (insert (propertize "Pinned" 'face 'chirp-social-context-face))
       (insert "\n")))
    (pcase-let (((map :retweeted-by :retweeted-by-name) tweet))
      (when retweeted-by
        (chirp-render--insert-prefix prefix prefix-face)
        (let ((action-start (point))
              (name (or retweeted-by-name retweeted-by)))
          (insert (propertize (format "retweeted by %s" name)
                              'face 'chirp-social-context-face))
          (chirp-render--add-profile-action
           action-start (point) retweeted-by
           (format "Open @%s" retweeted-by)))
        (insert "\n")))
    (unless trailing-newline-p
      (when (and (< start (point))
                 (eq (char-before) ?\n))
        (delete-char -1)))))

(cl-defun chirp-render--insert-tweet-body
    (tweet &key prefix prefix-face reply-context-prefix show-reply-context
           article-mode media-presentation (write-actions-p t)
           (trailing-newlines 1))
  "Insert TWEET content and actions.

PREFIX and PREFIX-FACE control indentation.  REPLY-CONTEXT-PREFIX overrides
PREFIX for the reply context.  SHOW-REPLY-CONTEXT controls the inline reply
target.  ARTICLE-MODE selects full article rendering when it is `full'.
MEDIA-PRESENTATION selects `track' or the default cover grid.  WRITE-ACTIONS-P
controls mutation actions.  TRAILING-NEWLINES controls the additional
newlines after the metrics row."
  (let* ((article-mode
          (if (or (eq article-mode 'full)
                  (chirp--tweet-expanded-p tweet))
              'full
            article-mode))
         (meta-start nil))
    (when-let* ((handle (and show-reply-context
                            (map-elt tweet :reply-to-handle))))
      (chirp-render--insert-prefix
       (or reply-context-prefix prefix) prefix-face)
      (insert (propertize "replying to "
                          'face 'chirp-thread-reply-context-face))
      (let ((handle-start (point)))
        (insert (propertize (format "@%s" handle)
                            'face 'chirp-link-face))
        (chirp-render--add-profile-action handle-start (point) handle))
      (insert "\n"))
    (pcase-let (((map :text :text-entities) tweet))
      (when (and text (not (string-empty-p text)))
        (chirp-render--insert-filled-text
         text prefix prefix-face text-entities)))
    (chirp-translate-insert tweet prefix prefix-face)
    (pcase article-mode
      ('full
       (chirp-render--insert-article-body tweet prefix prefix-face))
      (_
       (chirp-render--insert-article-preview
        tweet article-mode prefix prefix-face)
       (chirp-render--insert-article-media-preview
        tweet article-mode prefix prefix-face)
       (when (chirp-render--article-expandable-p tweet article-mode)
         (chirp-render--insert-show-more tweet prefix prefix-face))))
    (chirp-render--insert-link-cards tweet prefix prefix-face)
    (chirp-render--insert-expanded-urls
     (chirp-render--trailing-urls tweet) prefix prefix-face)
    (chirp-render-insert-media-strip
     (plist-get tweet :media) prefix prefix-face media-presentation)
    (chirp-render--insert-poll tweet prefix write-actions-p)
    (chirp-render--insert-quoted-tweet
     tweet prefix prefix-face write-actions-p)
    (chirp-render--insert-reply-control tweet prefix prefix-face)
    (setq meta-start (point))
    (pcase-let (((map :reply-count :reply-limited-p :retweet-count :retweeted-p
                      :like-count :liked-p :quote-count :bookmark-count
                      :bookmarked-p :view-count) tweet))
      (chirp-render--insert-metric
       'reply reply-count
       :action (and write-actions-p (not reply-limited-p) #'chirp-reply-at-point)
       :help-echo "Reply")
      (insert "   ")
      (chirp-render--insert-metric
       'retweet retweet-count :active retweeted-p
       :action (and write-actions-p #'chirp-toggle-retweet-at-point)
       :help-echo "Repost")
      (insert "   ")
      (chirp-render--insert-metric
       'like like-count :active liked-p
       :action (and write-actions-p #'chirp-toggle-like-at-point)
       :help-echo "Like")
      (insert "   ")
      (chirp-render--insert-metric
       'quote quote-count
       :action (and write-actions-p #'chirp-quote-at-point)
       :help-echo "Quote")
      (insert "   ")
      (chirp-render--insert-metric
       'bookmark bookmark-count :active bookmarked-p
       :action (and write-actions-p #'chirp-toggle-bookmark-at-point)
       :help-echo "Bookmark")
      (insert "   ")
      (chirp-render--insert-metric 'view view-count))
    (insert "\n")
    (dotimes (_ trailing-newlines)
      (insert "\n"))
    (put-text-property meta-start (point) 'rear-nonsticky t)))

(cl-defun chirp-render--insert-tweet
    (tweet &key prefix prefix-face show-reply-context article-mode reply-parent
           media-presentation (write-actions-p t) (time-format 'compact))
  "Insert TWEET at point, optionally prefixed for thread rendering.

PREFIX and PREFIX-FACE control indentation.  SHOW-REPLY-CONTEXT controls the
reply target.  ARTICLE-MODE controls full article rendering, and REPLY-PARENT
supplies the preceding parent tweet.  MEDIA-PRESENTATION selects the media
layout.  WRITE-ACTIONS-P controls mutation actions, while TIME-FORMAT selects
`compact' or `full' timestamps."
  (let ((start (point)))
    (chirp-render--insert-tweet-context
     tweet
     :prefix prefix
     :prefix-face prefix-face
     :reply-parent reply-parent)
    (chirp-render--insert-tweet-heading
     tweet
     :prefix prefix
     :prefix-face prefix-face
     :avatar-p t
     :time-format time-format)
    (chirp-render--insert-tweet-body
     tweet
     :prefix prefix
     :prefix-face prefix-face
     :show-reply-context show-reply-context
     :article-mode article-mode
     :media-presentation media-presentation
     :write-actions-p write-actions-p)
    (chirp-render--mark-entry start (point) tweet)))

(defun chirp-render-insert-tweet (tweet)
  "Insert TWEET at point."
  (chirp-render--insert-tweet tweet
                              :media-presentation 'carousel))

(defun chirp-render-insert-edit-history-row (row)
  "Insert one normalized edit-history ROW."
  (pcase-let* (((map :latest-p :key (:tweet original) :section) row)
               (tweet (copy-sequence original))
               (start (point)))
    (when section
      (insert (propertize section 'face 'chirp-author-face))
      (insert "\n\n"))
    (setq tweet (plist-put tweet :edited-p nil))
    (unless latest-p
      (setq tweet (plist-put tweet :kind 'edit-history-version)))
    (chirp-render--insert-tweet
     tweet
     :article-mode 'full
     :media-presentation 'carousel
     :write-actions-p latest-p
     :time-format 'full)
    (add-text-properties
     start (point)
     `(chirp-entry-id ,key
       chirp-entry-item ,tweet
       rear-nonsticky t))
    (cons start (point))))

(defun chirp-render-insert-discussion-entry (row)
  "Insert normalized discussion ROW and return its buffer span.

ROW contains `:key', `:parent-key', `:depth', `:role', `:connector',
`:focus-p', and `:tweet'.  Appkit owns the threaded geometry; Chirp owns
tweet content and actions."
  (pcase-let* (((map :tweet :key :focus-p :role :depth :parent-key :connector) row)
               (show-reply-context
                (and (eq role 'tree) (> (or depth 0) 1)))
         (span
          (appkit-discussion-insert-entry
           (appkit-discussion-entry-create
            :key key
            :parent-key parent-key
            :depth depth
            :connector connector
            :avatar nil
            :avatar-fallback " "
            :context-inserter
            (lambda ()
              (chirp-render--insert-tweet-context
               tweet
               :trailing-newline-p nil))
            :heading-inserter
            (lambda ()
              (chirp-render--insert-tweet-heading
               tweet
               :avatar-p t
               :time-p nil
               :newline-p nil))
            :time (if focus-p
                      (chirp-time-format-full
                       (plist-get tweet :created-at))
                    (chirp-time-format-compact
                     (plist-get tweet :created-at)))
            :body-inserter
            (lambda (body-prefix _properties)
              (let ((body-start (point)))
                (chirp-render--insert-tweet-body
                 tweet
                 :prefix nil
                 :reply-context-prefix nil
                 :show-reply-context show-reply-context
                 :article-mode (and focus-p 'full)
                 :media-presentation 'carousel
                 :trailing-newlines 0)
                (appkit-ui-apply-line-prefix
                 body-start (point) body-prefix)))
            :properties
            (list 'chirp-entry-item tweet
                  'chirp-entry-id key
                  'chirp-entry-url (plist-get tweet :url)
                  'rear-nonsticky t))
           :width (chirp--view-width)
           :avatar-p nil)))
    (put-text-property (car span) (1+ (car span))
                       'chirp-entry-start t)
    span))

;;; Rows and Users

;;;; Tweet Rows

(defun chirp-render--tweet-row-key (tweet)
  "Return the stable projection key for TWEET."
  (when-let* ((key (chirp-tweet-key tweet)))
    (if-let* ((entry-id (plist-get tweet :timeline-entry-id)))
        (list 'tweet key entry-id)
      (list 'tweet key))))

(defun chirp-render--tweet-row-dependencies (tweet)
  "Return TWEET's content dependencies, independent of repost row identity."
  (cons (list 'tweet (plist-get tweet :id))
        (chirp-media-resource-keys-for-tweet tweet)))

(defun chirp-render-project-tweet-rows (tweets)
  "Project normalized TWEETS into keyed Appkit rows."
  (appkit-projection-project
   tweets #'chirp-render--tweet-row-key
   :context-function (lambda (previous _tweet) previous)
   :dependencies-function #'chirp-render--tweet-row-dependencies))

(defun chirp-render-print-tweet-row (row)
  "Insert one projected tweet ROW at point."
  (let ((start (chirp-render-insert-tweet-row
                (appkit-projection-row-payload row)
                (appkit-projection-row-context row))))
    (put-text-property
     start (point) 'chirp-entry-id (appkit-projection-row-key row))))

(defun chirp-render--tweet-separator-line ()
  "Return the tweet separator line, or nil when disabled."
  (when (and (stringp chirp-tweet-separator)
             (not (string-empty-p chirp-tweet-separator)))
    (concat (make-string (max 0 chirp-tweet-separator-indent) ?\s)
            chirp-tweet-separator)))

(defun chirp-render-insert-tweet-separator ()
  "Insert the configured separator between tweet list entries."
  (when-let* ((line (chirp-render--tweet-separator-line)))
    (insert (propertize line 'face 'chirp-tweet-separator-face))
    (insert "\n\n")))

(defun chirp-render-insert-tweet-row (tweet previous)
  "Insert TWEET as a list row following PREVIOUS.

The row owns its preceding separator and direct-reply context, so keyed
projections can replace it as one unit.  Return the post's start, excluding
the separator so position anchors survive its insertion or removal."
  (when previous
    (chirp-render-insert-tweet-separator))
  (let ((start (point)))
    (if-let* ((reply-parent (chirp-render--list-reply-parent tweet previous)))
        (chirp-render--insert-tweet
         tweet
         :prefix chirp-render-list-reply-prefix
         :reply-parent reply-parent
         :media-presentation 'carousel)
      (chirp-render-insert-tweet tweet))
    start))

;;;; User Rows

(defun chirp-render-insert-user-summary (user)
  "Insert USER summary."
  (pcase-let* (((map :avatar-url :handle :name :bio :viewer-following-p
                     :viewer-followed-by-p :posts :following :followers
                     :joined :profile-url) user)
               (start (point)))
    (chirp-render--insert-avatar avatar-url handle)
    (let ((name-start (point)))
      (insert (propertize (or name "Unknown")
                          'face 'chirp-author-face))
      (when handle
        (insert " ")
        (insert (propertize (format "@%s" handle) 'face 'chirp-handle-face)))
      (chirp-render--add-profile-action name-start (point) handle))
    (insert "\n")
    (when bio
      (unless (string-empty-p bio)
        (chirp-render--insert-filled-text bio)))
    (when-let* ((action-label (chirp-render--profile-follow-action-label user))
                (handle handle))
      (let ((action-start (point)))
        (insert (propertize action-label 'face 'chirp-profile-action-face))
        (chirp-render--mark-profile-action-region
         action-start (point) 'toggle-follow handle))
      (when (and viewer-following-p viewer-followed-by-p)
        (insert (propertize "  Mutuals" 'face 'chirp-profile-action-secondary-face)))
      (insert "\n"))
    (insert (propertize (format "Posts %s" (chirp-format-count posts))
                        'face 'chirp-meta-face))
    (insert (propertize "   " 'face 'chirp-meta-face))
    (let ((following-start (point)))
      (insert (propertize
               (format "Following %s" (chirp-format-count following))
               'face 'chirp-meta-face))
      (chirp-render--mark-profile-list-region
       following-start (point) 'following handle))
    (insert (propertize "   " 'face 'chirp-meta-face))
    (let ((followers-start (point)))
      (insert (propertize
               (format "Followers %s" (chirp-format-count followers))
               'face 'chirp-meta-face))
      (chirp-render--mark-profile-list-region
       followers-start (point) 'followers handle))
    (insert "\n")
    (when joined
      (insert (propertize (format "Joined %s" joined) 'face 'chirp-meta-face))
      (insert "\n"))
    (when-let* ((url profile-url))
      (let ((url-start (point)))
        (insert (propertize url 'face 'link))
        (chirp-render--mark-url-region url-start (point) url)
        (insert "\n")))
    (insert "\n")
    (chirp-render--mark-entry start (point) user)))

(provide 'chirp-render)

;;; chirp-render.el ends here
