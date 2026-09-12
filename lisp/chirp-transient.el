;;; chirp-transient.el --- Discoverable Chirp menus -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Application destinations and operations on the tweet at point.

;;; Code:

(require 'transient)

(autoload 'chirp-home "chirp" nil t)
(autoload 'chirp-following "chirp" nil t)
(autoload 'chirp-bookmarks "chirp" nil t)
(autoload 'chirp-likes "chirp" nil t)
(autoload 'chirp-list "chirp" nil t)
(autoload 'chirp-me "chirp" nil t)
(autoload 'chirp-unsent-drafts "chirp-unsent" nil t)
(autoload 'chirp-unsent-scheduled "chirp-unsent" nil t)
(autoload 'chirp-edit-history-open-at-point "chirp-edit-history" nil t)

(autoload 'chirp-compose-post "chirp-actions" nil t)
(autoload 'chirp-reply-at-point "chirp-actions" nil t)
(autoload 'chirp-quote-at-point "chirp-actions" nil t)
(autoload 'chirp-toggle-retweet-at-point "chirp-actions" nil t)
(autoload 'chirp-follow-user-at-point "chirp-actions" nil t)
(autoload 'chirp-unfollow-user-at-point "chirp-actions" nil t)
(autoload 'chirp-toggle-like-at-point "chirp-actions" nil t)
(autoload 'chirp-toggle-bookmark-at-point "chirp-actions" nil t)
(autoload 'chirp-delete-at-point "chirp-actions" nil t)
(autoload 'chirp-translate-at-point "chirp-actions" nil t)
(autoload 'chirp-copy-fixupx-url-at-point "chirp-actions" nil t)
(autoload 'chirp-open-entry-at-point "chirp-core" nil t)
(autoload 'chirp-browse-at-point "chirp-core" nil t)

;;;###autoload(autoload 'chirp-transient-tweet-operate "chirp-transient" nil t)
(transient-define-prefix chirp-transient-tweet-operate ()
  "Operate on the tweet at point."
  [["Tweet"
    ("t" "Open context" chirp-open-entry-at-point)
    ("r" "Reply" chirp-reply-at-point)
    ("Q" "Quote" chirp-quote-at-point)
    ("R" "Retweet" chirp-toggle-retweet-at-point)
    ("H" "Edit history" chirp-edit-history-open-at-point)]
   ["Engage"
    ("l" "Like" chirp-toggle-like-at-point)
    ("B" "Bookmark" chirp-toggle-bookmark-at-point)]
   ["Other"
    ("d" "Delete" chirp-delete-at-point)
    ("T" "Translate" chirp-translate-at-point)
    ("y" "Copy fixupx" chirp-copy-fixupx-url-at-point)
    ("o" "Browser" chirp-browse-at-point)]])

;;;###autoload(autoload 'chirp-dispatch "chirp-transient" nil t)
(transient-define-prefix chirp-dispatch ()
  "Show Chirp destinations and composition actions."
  [["Timeline"
    ("h" "For You" chirp-home)
    ("f" "Following" chirp-following)
    ("u" "Me" chirp-me)
    ("b" "Bookmarks" chirp-bookmarks)
    ("L" "Liked" chirp-likes)
    ("s" "List" chirp-list)]
   ["Compose"
    ("c" "Post" chirp-compose-post)
    ("d" "Drafts" chirp-unsent-drafts)
    ("t" "Scheduled" chirp-unsent-scheduled)]
   ["Tweet"
    ("o" "Operate" chirp-transient-tweet-operate)]
   ["People"
    ("+" "Follow" chirp-follow-user-at-point)
    ("-" "Unfollow" chirp-unfollow-user-at-point)]])

(provide 'chirp-transient)

;;; chirp-transient.el ends here
