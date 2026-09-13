;;; chirp-transient.el --- Discoverable Chirp menus -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Application destinations and operations on the tweet at point.

;;; Code:

(require 'transient)
(require 'chirp-actions)
(require 'chirp-unsent)
(require 'chirp-edit-history)

;;;###autoload(autoload 'chirp-transient-tweet-operate "chirp" nil t)
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

;;;###autoload(autoload 'chirp-dispatch "chirp" nil t)
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
