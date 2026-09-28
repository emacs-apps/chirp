;;; chirp-profile-test.el --- Tests for Chirp profile loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp)
(require 'chirp-profile)
(require 'chirp-timeline)

(ert-deftest chirp-profile-open-starts-user-and-post-requests-in-parallel ()
  "Profile loading should request user metadata and posts concurrently."
  (let ((buffer (generate-new-buffer " *chirp-profile-test*"))
        user-callback
        posts-called)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (_callback &optional _errback)
                     nil))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle _callback &optional _errback _max-results _cursor)
                     (setq posts-called t)))
                  ((symbol-function 'chirp-display-buffer) #'ignore))
          (setq buffer (chirp-profile-open "alice"))
          (should (functionp user-callback))
          (should posts-called))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-follow-toggle-preserves-loaded-content ()
  "Follow actions must preserve loaded posts, pagination, and reader position."
  (let ((chirp--app nil)
        (chirp-backend-read-cache-ttl 15)
        (post-requests 0)
        (tweets (list '(:kind tweet :id "1" :text "Retained post"
                       :author-handle "alice")))
        server-following buffer callback errback path)
    (cl-labels
        ((raw-user ()
           `(("id_str" . "42") ("screen_name" . "alice") ("name" . "Alice")
             ("following" . ,server-following) ("followed_by" . t)
             ("friends_count" . 23)
             ("followers_count" . ,(if server-following 101 100))))
         (complete ()
           (setq server-following
                 (pcase path
                   ("1.1/friendships/create.json" t)
                   ("1.1/friendships/destroy.json" nil)
                   (_ (error "Unexpected request: %s" path))))
           (funcall callback (raw-user))))
      (unwind-protect
          (save-window-excursion
            (cl-letf
                (((symbol-function 'chirp-x-graphql-request)
                  (lambda (_operation _variables success &rest _options)
                    (funcall success
                             `(("data" . (("user" . (("result" . ,(raw-user))))))))))
                 ((symbol-function 'chirp-x-api-request)
                  (lambda (_service request-path success &rest options)
                    (setq path request-path
                          callback success
                          errback (plist-get options :errback))))
                 ((symbol-function 'chirp-backend-whoami)
                  (lambda (success &optional _errback)
                    (funcall success '(:kind user :handle "viewer") nil)))
                 ((symbol-function 'chirp-backend-user-posts)
                  (lambda (_handle success &optional _errback _limit _cursor)
                    (cl-incf post-requests)
                    (funcall success tweets
                             '(("pagination" . (("nextCursor" . "older")))))))
                 ((symbol-function 'chirp-display-buffer) #'ignore)
                 ((symbol-function 'chirp-media-prefetch-user) #'ignore)
                 ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                 ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
              (setq buffer (chirp-profile-open "alice"))
              (switch-to-buffer buffer)
              (cl-labels
                  ((drain ()
                     (let ((loop (appkit-surface-loop (appkit-current-surface))))
                       (while (> (appkit-loop-pending-count loop) 0)
                         (appkit-loop-run-pass loop))))
                   (show-action ()
                     (drain)
                     (goto-char
                      (or (text-property-any
                           (point-min) (point-max)
                           'chirp-profile-action 'toggle-follow)
                          (error "Profile has no follow action")))
                     (chirp-entry-at-point)))
                (should-not (plist-get (show-action) :viewer-following-p))
                (let ((page (copy-tree
                             (plist-get (chirp--projection-state) :page))))
                  (chirp-open-at-point)
                  ;; The response arrives after the reader moves into a post.
                  (search-forward "Retained post")
                  (let* ((position (point-marker))
                         (anchor (make-overlay (1- (point)) (point))))
                    (overlay-put anchor 'evaporate t)
                    (set-window-start (selected-window) (point) t)
                    (should-not (plist-get chirp-profile--user :viewer-following-p))
                    (complete)
                    (should-not
                     (plist-get (plist-get (chirp--projection-state) :user)
                                :viewer-following-p))
                    (drain)
                    (should (equal (plist-get (chirp--projection-state) :items)
                                   tweets))
                    (should (equal (plist-get (chirp--projection-state) :page)
                                   page))
                    (should (equal chirp--timeline-next-cursor "older"))
                    (should (= (point) (marker-position position)))
                    (should (= (window-start) (marker-position position)))
                    (should (eq (overlay-buffer anchor) buffer))
                    (delete-overlay anchor)
                    (set-marker position nil))
                  (should (plist-get (show-action) :viewer-following-p))
                  (should (= (plist-get chirp-profile--user :followers) 101))
                  (should (plist-get chirp-profile--user :viewer-followed-by-p))
                  ;; A failed unfollow must leave the confirmed relationship alone.
                  (chirp-open-at-point)
                  (funcall errback "Follow action rejected")
                  (should (plist-get (show-action) :viewer-following-p))
                  (should (= (plist-get chirp-profile--user :followers) 101))
                  (chirp-open-at-point)
                  (complete)
                  (should-not server-following)
                  (should-not (plist-get (show-action) :viewer-following-p))
                  (should (= (plist-get chirp-profile--user :followers) 100))
                  (should (equal (plist-get (chirp--projection-state) :items)
                                 tweets))
                  (should (equal (plist-get (chirp--projection-state) :page)
                                 page))
                  ;; A successful follow request can still await account approval.
                  (chirp-open-at-point)
                  (funcall callback (raw-user))
                  (should-not (plist-get (show-action) :viewer-following-p))
                  (should (= (plist-get chirp-profile--user :followers) 100))
                  (should (= post-requests 1))
                  ;; The same buffer can belong to a new Surface before completion.
                  (chirp-open-at-point)
                  (let* ((surface (appkit-current-surface))
                         (type (appkit-surface-type surface))
                         (input (copy-tree (chirp--projection-state))))
                    (appkit-surface-stop surface)
                    (let ((replacement
                           (appkit-open-generated-surface
                            type :app (chirp-app) :identity '(profile "alice")
                            :input input :buffer buffer)))
                      (funcall callback '(("following" . t)))
                      (drain)
                      (should-not
                       (plist-get
                        (plist-get (appkit-surface-model replacement) :user)
                        :viewer-following-p))))))))
        (chirp-stop)
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-profile-follow-updates-only-the-target-user-row ()
  "A user-list follow must update its target even after point moves away."
  (let* ((chirp--app nil)
         (users (list (list :kind 'user :id "42" :handle "Alice"
                            :viewer-following-p nil :followers 10)
                      (list :kind 'user :id "43" :handle "bob"
                            :viewer-following-p nil :followers 20)))
         (requests 0)
         buffer callback)
    (unwind-protect
        (cl-letf
            (((symbol-function 'chirp-backend-followers)
              (lambda (_handle success &optional _errback)
                (cl-incf requests)
                (funcall success users nil)))
             ((symbol-function 'chirp-backend-request)
              (lambda (_args success &optional _errback)
                (setq callback success)))
             ((symbol-function 'chirp-display-buffer) #'ignore)
             ((symbol-function 'chirp-media-prefetch-user) #'ignore))
          (setq buffer (chirp-profile-open-followers "viewer"))
          (with-current-buffer buffer
            (cl-labels
                ((drain ()
                   (let ((loop (appkit-surface-loop (appkit-current-surface))))
                     (while (> (appkit-loop-pending-count loop) 0)
                       (appkit-loop-run-pass loop)))))
              (drain)
              (goto-char
               (text-property-any (point-min) (point-max)
                                  'chirp-profile-action 'toggle-follow))
              (chirp-open-at-point)
              (search-forward "@bob")
              (let ((position (point-marker))
                    (anchor (make-overlay (1- (point)) (point))))
                (overlay-put anchor 'evaporate t)
                (funcall callback '(("following" . t)) nil)
                (should-not
                 (plist-get (car (plist-get (chirp--projection-state) :items))
                            :viewer-following-p))
                (drain)
                (should (= requests 1))
                (let ((items (plist-get (chirp--projection-state) :items)))
                  (should (plist-get (car items) :viewer-following-p))
                  (should (= (plist-get (car items) :followers) 11))
                  (should-not (plist-get (cadr items) :viewer-following-p))
                  (should (= (plist-get (cadr items) :followers) 20)))
                (should (= (point) (marker-position position)))
                (should (eq (overlay-buffer anchor) buffer))
                (delete-overlay anchor)
                (set-marker position nil))
              (goto-char
               (text-property-any (point-min) (point-max)
                                  'chirp-profile-action 'toggle-follow))
              (should (plist-get (chirp-entry-at-point) :viewer-following-p)))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-open-followers-disables-wrap-navigation ()
  "Follower/following list buffers should stop at the ends instead of wrapping."
  (let ((chirp--app nil) (buffer (generate-new-buffer " *chirp-profile-followers-test*"))
        followers-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-followers)
                   (lambda (_handle callback &optional _errback)
                     (setq followers-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (_callback &optional _errback)
                     nil))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore))
          (setq buffer (chirp-profile-open-followers "alice"))
          (should (functionp followers-callback))
          (funcall followers-callback (list '(:kind user :handle "bob")) nil)
          (with-current-buffer buffer
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (should-not chirp--entry-wrap-navigation)))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-open-renders-header-before-posts-arrive ()
  "Profile view should show the header immediately without a Recent Posts section."
  (let ((chirp--app nil) (buffer (generate-new-buffer " *chirp-profile-header-test*"))
        user-callback
        posts-callback
        whoami-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-callback callback)))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle callback &optional _errback _max-results _cursor)
                     (setq posts-callback callback)))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (setq buffer (chirp-profile-open "alice"))
          (funcall user-callback
                   '(:kind user :handle "alice" :name "Alice" :bio "" :posts 12 :following 3 :followers 4)
                   nil)
          (with-current-buffer buffer
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (should (string-match-p "@alice" (buffer-string)))
            (should (string-match-p "Loading posts..." (buffer-string)))
            (should-not (string-match-p "Recent Posts" (buffer-string))))
          (funcall whoami-callback '(:kind user :handle "alice") nil)
          (funcall posts-callback
                   (list '(:kind tweet :id "1" :text "hello" :author-handle "alice"))
                   '(("pagination" . (("nextCursor" . "cursor-next")))))
          (with-current-buffer buffer
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (should (equal chirp-profile--available-modes '(posts replies highlights media likes)))
            (should (eq chirp--timeline-load-more-function #'chirp-profile-load-more))
            (should (equal chirp--timeline-next-cursor "cursor-next"))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-load-more-appends-older-posts ()
  "Loading more in a profile should append older tweets using the next cursor."
  (let ((chirp--app nil) (buffer (generate-new-buffer " *chirp-profile-load-more*"))
        user-callback
        initial-callback
        whoami-callback
        older-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-begin-request)
                   (lambda (_buffer)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-callback callback)))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle callback &optional _errback _max-results cursor)
                     (if cursor
                         (setq older-callback callback)
                       (setq initial-callback callback))))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (setq buffer (chirp-profile-open "alice"))
          (funcall user-callback
                   '(:kind user :handle "alice" :name "Alice" :bio ""
                     :posts 12 :following 3 :followers 4)
                   nil)
          (funcall whoami-callback '(:kind user :handle "bob") nil)
          (funcall initial-callback
                   (list '(:kind tweet :id "1" :text "first"
                           :author-handle "alice"))
                   '(("pagination" . (("nextCursor" . "cursor-prev")))))
          (with-current-buffer buffer
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (chirp-profile-load-more)
            (funcall older-callback
                     (list '(:kind tweet :id "2" :text "second"
                             :author-handle "alice"))
                     '(("pagination" . (("nextCursor" . "cursor-next")))))
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (should (string-match-p "first" (buffer-string)))
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (should (string-match-p "second" (buffer-string)))
            (should (equal chirp--timeline-next-cursor "cursor-next"))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-load-more-uses-current-subview-fetcher ()
  "Loading more should use the active profile subview command."
  (let ((chirp--app nil) (buffer (generate-new-buffer " *chirp-profile-load-more-replies*"))
        user-callback
        initial-callback
        whoami-callback
        older-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-begin-request)
                   (lambda (_buffer)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-callback callback)))
                  ((symbol-function 'chirp-backend-user-replies)
                   (lambda (_handle callback &optional _errback _max-results cursor)
                     (if cursor
                         (setq older-callback callback)
                       (setq initial-callback callback))))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (setq buffer (chirp-profile-open "alice" 'replies))
          (funcall user-callback
                   '(:kind user :handle "alice" :name "Alice" :bio ""
                     :posts 12 :following 3 :followers 4)
                   nil)
          (funcall whoami-callback '(:kind user :handle "bob") nil)
          (funcall initial-callback
                   (list '(:kind tweet :id "1" :text "first reply"
                           :author-handle "alice"))
                   '(("pagination" . (("nextCursor" . "cursor-prev")))))
          (with-current-buffer buffer
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (chirp-profile-load-more)
            (funcall older-callback
                     (list '(:kind tweet :id "2" :text "second reply"
                             :author-handle "alice"))
                     '(("pagination" . (("nextCursor" . "cursor-next")))))
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (should (string-match-p "first reply" (buffer-string)))
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (should (string-match-p "second reply" (buffer-string)))
            (should (equal chirp--timeline-next-cursor "cursor-next"))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-open-adds-likes-mode-for-own-profile ()
  "Own profiles should expose a Likes mode in the profile strip."
  (let ((chirp--app nil) (buffer (generate-new-buffer " *chirp-profile-likes-mode*"))
        user-callback
        posts-callback
        whoami-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (target _title)
                     (with-current-buffer target
                       (setq-local chirp--request-token 'profile-token))
                     'profile-token))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-callback callback)))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle callback &optional _errback _max-results _cursor)
                     (setq posts-callback callback)))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (setq buffer (chirp-profile-open "alice"))
          (funcall user-callback
                   '(:kind user :handle "alice" :name "Alice" :bio "" :posts 12 :following 3 :followers 4)
                   nil)
          (funcall posts-callback
                   (list '(:kind tweet :id "1" :text "hello" :author-handle "alice"))
                   nil)
          (funcall whoami-callback '(:kind user :handle "alice") nil)
          (with-current-buffer buffer
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (goto-char (point-min))
            (should (search-forward "Posts" nil t))
            (should (search-forward "Replies" nil t))
            (should (search-forward "Highlights" nil t))
            (should (search-forward "Media" nil t))
            (should (search-forward "Likes" nil t))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-open-adds-likes-mode-case-insensitively ()
  "Own profile detection should ignore handle case when exposing Likes."
  (let ((chirp--app nil) (buffer (generate-new-buffer " *chirp-profile-likes-case*"))
        user-callback
        posts-callback
        whoami-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'profile-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'profile-token)))
                  ((symbol-function 'chirp-backend-user)
                   (lambda (_handle callback &optional _errback)
                     (setq user-callback callback)))
                  ((symbol-function 'chirp-backend-whoami)
                   (lambda (callback &optional _errback)
                     (setq whoami-callback callback)))
                  ((symbol-function 'chirp-backend-user-posts)
                   (lambda (_handle callback &optional _errback _max-results _cursor)
                     (setq posts-callback callback)))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-user) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (setq buffer (chirp-profile-open "lucius_chen"))
          (funcall user-callback
                   '(:kind user :handle "lucius_chen" :name "Lucius" :bio "" :posts 12 :following 3 :followers 4)
                   nil)
          (funcall posts-callback
                   (list '(:kind tweet :id "1" :text "hello" :author-handle "lucius_chen"))
                   nil)
          (funcall whoami-callback '(:kind user :handle "Lucius_Chen") nil)
          (with-current-buffer buffer
            (let ((loop (appkit-surface-loop (appkit-current-surface))))
              (while (> (appkit-loop-pending-count loop) 0)
                (appkit-loop-run-pass loop)))
            (should (equal chirp-profile--available-modes '(posts replies highlights media likes)))))
      (chirp-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-profile-mode-strip-ret-switches-profile-view ()
  "RET on a profile mode label should call the profile switch function."
  (with-temp-buffer
    (chirp-view-mode)
    (let (captured)
      (setq-local chirp--profile-switch-mode-function
                  (lambda (mode)
                    (setq captured mode)))
      (let ((inhibit-read-only t))
        (chirp-render-insert-profile-view-strip 'posts '(posts replies highlights media likes)))
      (goto-char (point-min))
      (search-forward "Likes")
      (backward-char 2)
      (chirp-open-at-point)
      (should (eq captured 'likes)))))

(ert-deftest chirp-profile-tab-cycles-profile-view ()
  "TAB should cycle profile subviews when a profile switch function exists."
  (with-temp-buffer
    (chirp-view-mode)
    (let (captured)
      (setq-local chirp--profile-switch-mode-function
                  (lambda (mode)
                    (setq captured mode)))
      (chirp-toggle-home-following)
      (should (eq captured :next)))))

(ert-deftest chirp-me-opens-the-authenticated-profile ()
  "The public Chirp entry point should resolve and open the current profile."
  (let (opened-handle)
    (cl-letf (((symbol-function 'chirp-backend-whoami)
               (lambda (callback &optional _errback)
                 (funcall callback '(:kind user :handle "alice") nil)))
              ((symbol-function 'chirp-profile-open)
               (lambda (handle &optional _mode)
                 (setq opened-handle handle))))
      (chirp-me)
      (should (equal opened-handle "alice")))))

(provide 'chirp-profile-test)

;;; chirp-profile-test.el ends here
