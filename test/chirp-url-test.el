;;; chirp-url-test.el --- Tests for Chirp X URL targets -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'appkit-url)
(require 'chirp-url)
(require 'chirp)

(ert-deftest chirp-url-parses-supported-x-destinations ()
  "Trusted X and legacy Twitter URLs should map to Chirp destinations."
  (dolist
      (case
       '(("https://x.com/alice/status/123" (:kind tweet :id "123"))
         ("https://www.twitter.com/alice/status/123/history?s=20"
          (:kind edit-history :id "123"))
         ("https://fixupx.com/alice/status/123" (:kind tweet :id "123"))
         ("https://www.fixupx.com/i/status/123" (:kind tweet :id "123"))
         ("https://fxtwitter.com/alice/status/123/video/1?s=20"
          (:kind tweet :id "123"))
         ("https://d.fxtwitter.com/alice/status/123" (:kind tweet :id "123"))
         ("https://vxtwitter.com/alice" (:kind profile :handle "alice"))
         ("https://twittpr.com/alice/status/123/history"
          (:kind edit-history :id "123"))
         ("https://x.com/i/web/status/123" (:kind tweet :id "123"))
         ("https://x.com/i/status/123" (:kind tweet :id "123"))
         ("https://x.com/alice" (:kind profile :handle "alice"))
         ("https://x.com/alice/followers"
          (:kind followers :handle "alice"))
         ("https://x.com/alice/following"
          (:kind following-users :handle "alice"))
         ("https://x.com/alice/likes" (:kind likes :handle "alice"))
         ("https://x.com/i/lists/456" (:kind list :id "456"))
         ("https://twitter.com/alice/lists/456" (:kind list :id "456"))
         ("https://x.com/search?q=emacs%20lisp&src=typed_query"
          (:kind search :query "emacs lisp"))
         ("https://x.com/home" (:kind home))
         ("https://x.com/i/bookmarks" (:kind bookmarks))
         ("https://x.com/messages" (:kind direct-messages))))
    (should (equal (chirp-url-parse (car case)) (cadr case)))))

(ert-deftest chirp-url-rejects-untrusted-or-unsupported-locations ()
  "Only supported HTTPS X locations should become Chirp targets."
  (dolist (url '("http://x.com/alice/status/123"
                 "https://x.com.evil.example/alice/status/123"
                 "https://user@x.com/alice/status/123"
                 "https://x.com:444/alice/status/123"
                 "https://mobile.twitter.com/alice/status/123"
                 "http://fixupx.com/alice/status/123"
                 "https://fixupx.com.evil.example/alice/status/123"
                 "https://x.com/search"
                 "https://x.com/notifications"
                 "not a url"))
    (should-not (chirp-url-parse url))))

(ert-deftest chirp-url-extractors-accept-identifiers-and-trusted-urls ()
  "Domain extractors should share the trusted URL parser."
  (should (equal (chirp-url-tweet-id "123") "123"))
  (should-not (chirp-url-tweet-id nil))
  (should (equal (chirp-url-tweet-id
                  "https://twitter.com/alice/status/123/photo/1")
                 "123"))
  (should (equal (chirp-url-tweet-id
                  '(:url "https://x.com/alice/status/123"))
                 "123"))
  (should-not (chirp-url-tweet-id
               "https://example.com/alice/status/123"))
  (should (equal (chirp-url-tweet-id
                  "https://fixupx.com/alice/status/123")
                 "123")))

(ert-deftest chirp-url-browse-regexp-routes-trusted-hosts ()
  "Browse routing should claim X links and their embed mirrors."
  (dolist (url '("https://x.com/alice/status/123"
                 "https://twitter.com/alice"
                 "https://fixupx.com/alice/status/123"
                 "https://fxtwitter.com/alice/status/123"))
    (should (string-match-p chirp-url-browse-regexp url)))
  (should-not (string-match-p chirp-url-browse-regexp
                              "https://fixupx.com.evil.example/alice"))
  (should-not (string-match-p chirp-url-browse-regexp
                              "http://fixupx.com/alice")))

(ert-deftest chirp-open-url-routes-each-target-through-public-command ()
  "The public URL command should dispatch every parsed destination."
  (let (calls)
    (cl-letf (((symbol-function 'chirp-timeline-open-home)
               (lambda () (push '(home) calls)))
              ((symbol-function 'chirp-timeline-open-bookmarks)
               (lambda () (push '(bookmarks) calls)))
              ((symbol-function 'chirp-dm-open-inbox)
               (lambda () (push '(direct-messages) calls)))
              ((symbol-function 'chirp-timeline-open-search)
               (lambda (query) (push (list 'search query) calls)))
              ((symbol-function 'chirp-thread-open)
               (lambda (target)
                 (push (list 'tweet target) calls)))
              ((symbol-function 'chirp-edit-history-open)
               (lambda (target) (push (list 'edit-history target) calls)))
              ((symbol-function 'chirp-profile-open)
               (lambda (handle &optional _mode)
                 (push (list 'profile handle) calls)))
              ((symbol-function 'chirp-profile-open-followers)
               (lambda (handle)
                 (push (list 'followers handle) calls)))
              ((symbol-function 'chirp-profile-open-following-users)
               (lambda (handle)
                 (push (list 'following-users handle) calls)))
              ((symbol-function 'chirp-timeline-open-likes)
               (lambda (&optional handle)
                 (push (list 'likes handle) calls)))
              ((symbol-function 'chirp-timeline-open-list)
               (lambda (&optional target)
                 (push (list 'list target) calls))))
      (dolist (url '("https://x.com/home"
                     "https://x.com/i/bookmarks"
                     "https://x.com/messages"
                     "https://x.com/search?q=emacs"
                     "https://x.com/alice/status/123"
                     "https://x.com/alice/status/123/history"
                     "https://fixupx.com/alice/status/123/photo/1"
                     "https://x.com/alice"
                     "https://x.com/alice/followers"
                     "https://x.com/alice/following"
                     "https://x.com/alice/likes"
                     "https://x.com/i/lists/456"))
        (chirp-open-url url t)))
    (should
     (equal (nreverse calls)
            '((home) (bookmarks) (direct-messages) (search "emacs")
              (tweet "123") (edit-history "123") (tweet "123")
              (profile "alice")
              (followers "alice") (following-users "alice")
              (likes "alice") (list "456"))))))

(ert-deftest chirp-open-url-rejects-unsupported-x-path ()
  "The public URL command should surface an unsupported X location."
  (should-error (chirp-open-url "https://x.com/notifications")
                :type 'user-error))

(ert-deftest chirp-url-handler-claims-only-supported-destinations ()
  "Chirp's registered predicate leaves unknown X pages and other hosts alone."
  (let ((appkit-url--handlers (list (assq 'chirp appkit-url--handlers)))
        (appkit-url-mode nil)
        (browse-url-handlers nil)
        (browse-url-default-handlers nil))
    (should (car appkit-url--handlers))
    (appkit-url-mode 1)
    (dolist (url '("https://x.com/alice/status/123"
                   "https://twitter.com/alice"
                   "https://fixupx.com/alice/status/123"
                   "https://x.com/search?q=emacs"
                   "https://x.com/home"))
      (should (eq (browse-url-select-handler url 'internal) #'chirp-open-url))
      (should-not (browse-url-select-handler url 'external)))
    (dolist (url '("https://x.com/"
                   "https://x.com/notifications"
                   "https://x.com/search"
                   "https://x.com.evil.example/alice/status/123"
                   "https://example.org/"))
      (should-not (browse-url-select-handler url)))
    (appkit-url-mode -1)
    (should-not (browse-url-select-handler "https://x.com/alice/status/123"))))

(ert-deftest chirp-browse-at-point-uses-an-external-browser ()
  "Opening the current entry in a browser cannot dispatch back into Chirp."
  (let (call)
    (cl-letf (((symbol-function 'chirp-media-at-point) (lambda () nil))
              ((symbol-function 'chirp-entry-at-point) (lambda () nil))
              ((symbol-function 'chirp--text-property-at-point)
               (lambda (&rest _) nil))
              ((symbol-function 'chirp-entry-url-at-point)
               (lambda () "https://x.com/alice/status/123"))
              ((symbol-function 'browse-url-with-browser-kind)
               (lambda (&rest args) (setq call args))))
      (chirp-browse-at-point))
    (should (equal call '(external "https://x.com/alice/status/123" nil)))))

(provide 'chirp-url-test)

;;; chirp-url-test.el ends here
