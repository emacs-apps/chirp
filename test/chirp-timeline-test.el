;;; chirp-timeline-test.el --- Tests for timeline loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-core)
(require 'chirp-timeline)
(require 'chirp-actions)

(defun chirp-timeline-test--drain (view)
  "Deliver all already queued work for VIEW without running unrelated timers."
  (let ((loop (appkit-surface-loop view)))
    (while (> (appkit-loop-pending-count loop) 0)
      (appkit-loop-run-pass loop))
    (should (eq (appkit-surface-status view) 'running))))

(ert-deftest chirp-buffer-creates-fresh-view-buffers ()
  "Each `chirp-buffer' call should return a fresh buffer."
  (let ((buffer-a (chirp-buffer))
        (buffer-b (chirp-buffer)))
    (unwind-protect
        (progn
          (should (buffer-live-p buffer-a))
          (should (buffer-live-p buffer-b))
          (should-not (eq buffer-a buffer-b))
          (chirp--apply-buffer-name buffer-a "For You")
          (should (string= (buffer-name buffer-a) "*chirp: For You*")))
      (dolist (buffer (list buffer-a buffer-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-primary-timeline-retains-both-feeds-in-one-view ()
  "Primary commands should reuse one view and both canonical feed states."
  (let ((chirp--app nil) buffer requests)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda
                  (callback &optional following _errback _max-results
                            _cursor owner)
                  (push
                   (list :callback callback :following following
                         :owner owner)
                   requests)))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (let*
                ((first-buffer (chirp-timeline-open-home))
                 (view
                  (with-current-buffer first-buffer
                    (appkit-current-surface))))
              (setq buffer first-buffer)
              (funcall (plist-get (car requests) :callback)
                       (list
                        '(:kind tweet :id "home" :text
                          "Retained\ndetail"))
                       '((pagination (nextCursor . "home-cursor"))))
              (appkit-loop-run-pass (appkit-surface-loop view))
              (let
                  ((home-state (appkit-surface-model view)) home-point)
                (with-current-buffer first-buffer
                  (goto-char (point-min)) (search-forward "detail")
                  (setq home-point (point)))
                (let ((second-buffer (chirp-timeline-open-home)))
                  (should (eq first-buffer second-buffer))
                  (should
                   (eq view
                       (with-current-buffer second-buffer
                         (appkit-current-surface))))
                  (should (= (length requests) 1))
                  (should
                   (equal (appkit-surface-identity view)
                          chirp-timeline--primary-view-id)))
                (with-current-buffer first-buffer
                  (should-not chirp--timeline-kind)
                  (chirp-toggle-home-following)
                  (should (eq view (appkit-current-surface))))
                (let ((following-state (appkit-surface-model view)))
                  (should-not (eq home-state following-state))
                  (should (= (length requests) 2))
                  (should (plist-get (car requests) :following)) nil
                  nil
                  (funcall (plist-get (car requests) :callback)
                           (list
                            '(:kind tweet :id "following" :text
                              "Following"))
                           '((pagination (nextCursor . "following-cursor"))))
                  (appkit-loop-run-pass (appkit-surface-loop view))
                  (with-current-buffer first-buffer
                    (chirp-toggle-home-following))
                  (should (eq (appkit-surface-model view) home-state))
                  (should (= (length requests) 2))
                  (appkit-loop-run-pass (appkit-surface-loop view))
                  (with-current-buffer first-buffer
                    (should
                     (equal (plist-get (chirp-entry-at-point) :id)
                            "home"))
                    (should (= (point) home-point)))
                  (should
                   (equal
                    (plist-get (plist-get home-state :page)
                               :next-cursor)
                    "home-cursor"))
                  (should
                   (eq
                    (gethash 'home
                             (chirp--session-primary-feed-states
                              (chirp--session)))
                    home-state))
                  (should
                   (eq (chirp-timeline-open-following) first-buffer))
                  (should
                   (eq (appkit-surface-model view) following-state))
                  (should (= (length requests) 2))
                  (should
                   (equal
                    (plist-get (plist-get following-state :page)
                               :next-cursor)
                    "following-cursor")))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-feed-state-survives-buffer-recreation ()
  "Killing the primary buffer should not discard session-owned feed state."
  (let ((chirp--app nil) buffer requests)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (callback &rest _args)
                  (push callback requests)))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let*
                ((first-view
                  (with-current-buffer buffer
                    (appkit-current-surface)))
                 (state (appkit-surface-model first-view)))
              (funcall (car requests)
                       (list
                        '(:kind tweet :id "1" :text "First\nentry")
                        '(:kind tweet :id "2" :text
                          "Retained\nposition")
                        '(:kind tweet :id "3" :text "Third\nentry"))
                       nil)
              (appkit-loop-run-pass (appkit-surface-loop first-view))
              (with-current-buffer buffer
                (goto-char (point-min)) (search-forward "position")
                (set-window-start (selected-window)
                                  (appkit-position-find-property-value
                                   (point-min) (point-max)
                                   'chirp-entry-id '(tweet "2"))
                                  t))
              (kill-buffer buffer)
              (should-not (appkit-surface-live-p first-view))
              (setq buffer (chirp-timeline-open-home))
              (let
                  ((second-view
                    (with-current-buffer buffer
                      (appkit-current-surface))))
                (should-not (eq first-view second-view))
                (should (eq state (appkit-surface-model second-view)))
                (should (= (length requests) 1))
                (with-current-buffer buffer
                  (should
                   (equal (plist-get (chirp-entry-at-point) :id) "2"))
                  (should (search-backward "position" nil t))
                  (should
                   (equal
                    (get-text-property
                     (window-start (selected-window)) 'chirp-entry-id)
                    '(tweet "2"))))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-recreated-view-rejects-old-callback ()
  "A callback from a killed view must not overwrite its restarted feed."
  (let ((chirp--app nil) buffer callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (callback &rest _args)
                  (setq callbacks (append callbacks (list callback)))))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let*
                ((first-view
                  (with-current-buffer buffer
                    (appkit-current-surface)))
                 (state (appkit-surface-model first-view)))
              (kill-buffer buffer)
              (setq buffer (chirp-timeline-open-home))
              (let ((second-generation (plist-get state :generation)))
                (should (= (length callbacks) 2))
                (funcall (nth 0 callbacks)
                         (list '(:kind tweet :id "old" :text "Old"))
                         nil)
                (should-not (plist-get state :items))
                (should
                 (eq (plist-get state :generation) second-generation))
                (funcall (nth 1 callbacks)
                         (list '(:kind tweet :id "new" :text "New"))
                         nil)
                (chirp-timeline-test--drain
                 (with-current-buffer buffer (appkit-current-surface)))
                (should
                 (equal (plist-get (car (plist-get state :items)) :id)
                        "new"))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))


(ert-deftest chirp-primary-superseding-request-cancels-old-transport ()
  "Starting a replacement generation cancels the prior retrieval buffer."
  (let ((chirp--app nil) buffer requests canceled)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (_callback &optional _following _errback _max-results _cursor owner)
                       (let ((request (generate-new-buffer " *chirp-feed-request*")))
                         (push request requests)
                         (appkit-register-handle owner 'function request #'chirp-x-cancel-request)
                         request)))
                    ((symbol-function 'chirp-x-cancel-request)
                     (lambda (request) (setq canceled request) (kill-buffer request)))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((first (car requests)))
              (with-current-buffer buffer (chirp-refresh))
              (should (eq canceled first))
              (should-not (buffer-live-p first))
              (should (buffer-live-p (car requests)))
              (should-not (eq first (car requests))))))
      (chirp-stop)
      (dolist (target (cons buffer requests))
        (when (buffer-live-p target) (kill-buffer target))))))

(ert-deftest
    chirp-primary-first-position-intent-survives-request-coalescing
    ()
  "A new request should not erase an unprojected move-to-first intent."
  (let ((chirp--app nil) buffer callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success &rest _args)
                  (setq callbacks (append callbacks (list success)))
                  (list 'request (length callbacks))))
               ((symbol-function 'chirp-x-cancel-request) #'ignore)
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let
                ((view
                  (with-current-buffer buffer
                    (appkit-current-surface))))
              (funcall (nth 0 callbacks)
                       (list '(:kind tweet :id "1" :text "First")) nil)
              (with-current-buffer buffer
                (goto-char (point-max)) (chirp-refresh))
              nil (appkit-loop-run-pass (appkit-surface-loop view))
              (with-current-buffer buffer
                (should
                 (equal (plist-get (chirp-entry-at-point) :id) "1")))
              nil)))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest
    chirp-primary-stale-generation-cannot-overwrite-newer-state ()
  "A superseded callback should settle without projecting its result."
  (let ((chirp--app nil) buffer callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success &rest _args)
                  (setq callbacks (append callbacks (list success)))))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let*
                ((view
                  (with-current-buffer buffer
                    (appkit-current-surface)))
                 (state (appkit-surface-model view))
                 (first-generation (plist-get state :generation)))
              (with-current-buffer buffer (chirp-refresh))
              (let ((second-generation (plist-get state :generation)))
                (should-not (eq first-generation second-generation))
                (funcall (nth 0 callbacks)
                         (list '(:kind tweet :id "old" :text "old"))
                         nil)
                (should
                 (eq (plist-get state :generation) second-generation))
                (should-not (plist-get state :items))
                (funcall (nth 1 callbacks)
                         (list '(:kind tweet :id "new" :text "new"))
                         nil)
                (chirp-timeline-test--drain view)
                (should
                 (equal (plist-get (car (plist-get state :items)) :id)
                        "new"))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest
    chirp-primary-visible-end-auto-loads-and-preserves-position ()
  "Reaching the visible end should quietly append an older page."
  (let
      ((chirp--app nil) (chirp-timeline-poll-interval nil)
       (chirp-timeline-auto-load-threshold 50) buffer requests
       rechecks)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda
                  (success following _errback _max-results cursor
                           owner)
                  (setq requests
                        (append requests
                                (list
                                 (list :success success :following
                                       following :cursor cursor :owner
                                       owner))))
                  (list 'request (length requests))))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let*
                ((view
                  (with-current-buffer buffer
                    (appkit-current-surface)))
                 (observer
                  (with-current-buffer buffer
                    chirp-timeline--scroll-observer)))
              (should (appkit-scroll-observer-p observer))
              (should (appkit-scroll-observer-active-p observer))
              (should
               (eq view (appkit-scroll-observer-owner observer)))
              (funcall (plist-get (nth 0 requests) :success)
                       (list
                        '(:kind tweet :id "1" :text "Visible post"))
                       '((pagination (nextCursor . "older-1"))))
              (appkit-loop-run-pass (appkit-surface-loop view))
              (with-current-buffer buffer
                (search-forward "Visible post"))
              (cl-letf
                  (((symbol-function 'appkit-scroll-observer-check)
                    (lambda (candidate &optional _window)
                      (should (eq candidate observer))
                      (setq rechecks (1+ (or rechecks 0))))))
                (with-current-buffer buffer
                  (funcall
                   (appkit-scroll-observer-end-function observer)
                   'window 975 1000))
                (should (= 2 (length requests)))
                (should
                 (equal (plist-get (nth 1 requests) :cursor) "older-1"))
                (funcall (plist-get (nth 1 requests) :success)
                         (list
                          '(:kind tweet :id "2" :text "Older post"))
                         '((pagination (nextCursor . "older-2"))))
                (appkit-loop-run-pass (appkit-surface-loop view))
                (should (= 1 rechecks)))
              (with-current-buffer buffer
                (should
                 (equal (plist-get (chirp-entry-at-point) :id) "1")))
              (should
               (equal
                (mapcar (lambda (tweet) (plist-get tweet :id))
                        (plist-get (appkit-surface-model view) :items))
                '("1" "2"))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-auto-load-continues-quietly-through-empty-page ()
  "An empty automatic page should advance after projection without announcing."
  (let ((chirp--app nil)
        (chirp-timeline-poll-interval nil)
        (chirp-timeline-auto-load-threshold 50)
        buffer messages requests deferred-check)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'chirp-backend-feed)
                     (lambda (success _following errback _max-results cursor
                                      _owner)
                       (setq requests
                             (append requests
                                     (list (list :success success :errback errback
                                                 :cursor cursor))))
                       (list 'request (length requests))))
                    ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                    ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
                    ((symbol-function 'message)
                     (lambda (format-string &rest arguments)
                       (when format-string
                         (push (apply #'format format-string arguments)
                               messages)))))
            (setq buffer (chirp-timeline-open-home))
            (let* ((view (with-current-buffer buffer (appkit-current-surface)))
                   (observer (with-current-buffer buffer
                               chirp-timeline--scroll-observer)))
              (funcall (plist-get (nth 0 requests) :success)
                       (list '(:kind tweet :id "1" :text "Visible post"))
                       '((pagination (nextCursor . "older-1"))))
              (while (> (appkit-loop-pending-count (appkit-surface-loop view)) 0)
                (appkit-loop-run-pass (appkit-surface-loop view)))
              (cl-letf (((symbol-function 'appkit-scroll-window-visible-range)
                         (lambda (_window start end) (cons start end)))
                        ((symbol-function 'run-with-idle-timer)
                         (lambda (_delay _repeat function &rest arguments)
                           (setq deferred-check (cons function arguments))
                           (timer-create))))
                (appkit-scroll-observer-check observer)
                (should (= 2 (length requests)))
                (setq messages nil)
                (funcall (plist-get (nth 1 requests) :success) nil
                         '((pagination (nextCursor . "older-2"))))
                (while (> (appkit-loop-pending-count (appkit-surface-loop view)) 0)
                  (appkit-loop-run-pass (appkit-surface-loop view)))
                (let ((page (plist-get (appkit-surface-model view) :page)))
                  (should (equal (plist-get page :next-cursor) "older-2"))
                  (should-not (plist-get page :auto-load-paused-p))
                  (should-not (plist-get page :exhausted-p)))
                (should (= 2 (length requests)))
                (let ((check deferred-check))
                  (setq deferred-check nil)
                  (apply (car check) (cdr check)))
                (should (= 3 (length requests)))
                (should (equal (plist-get (nth 2 requests) :cursor) "older-2"))
                (should-not messages)
                (funcall (plist-get (nth 2 requests) :success)
                         (list '(:kind tweet :id "2" :text "Older post"))
                         '((pagination (nextCursor . "older-3"))))
                (chirp-timeline-test--drain view)
                (should
                 (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                                (plist-get (appkit-surface-model view) :items))
                        '("1" "2")))))))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-auto-load-errors-remain-quiet-and-paused
    ()
  "An automatic paging failure should wait for an explicit retry."
  (let
      ((chirp--app nil) (chirp-timeline-poll-interval nil)
       (chirp-timeline-auto-load-threshold 50) buffer messages
       requests)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda
                  (success _following errback _max-results cursor
                           _owner)
                  (setq requests
                        (append requests
                                (list
                                 (list :success success :errback
                                       errback :cursor cursor))))
                  (list 'request (length requests))))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
               ((symbol-function 'message)
                (lambda (format-string &rest arguments)
                  (when format-string
                    (push (apply #'format format-string arguments)
                          messages)))))
            (setq buffer (chirp-timeline-open-home))
            (let*
                ((view
                  (with-current-buffer buffer
                    (appkit-current-surface)))
                 (observer
                  (with-current-buffer buffer
                    chirp-timeline--scroll-observer)))
              (funcall (plist-get (nth 0 requests) :success)
                       (list
                        '(:kind tweet :id "1" :text "Visible post"))
                       '((pagination (nextCursor . "older-1"))))
              (appkit-loop-run-pass (appkit-surface-loop view))
              (with-current-buffer buffer
                (funcall
                 (appkit-scroll-observer-end-function observer)
                 'window 975 1000))
              (should (= 2 (length requests)))
              (funcall (plist-get (nth 1 requests) :errback)
                       "Older page failed")
              (appkit-loop-run-pass (appkit-surface-loop view))
              (let*
                  ((state (appkit-surface-model view))
                   (page (plist-get state :page)))
                (should
                 (eq (plist-get (plist-get state :status) :phase)
                     'idle))
                (should (plist-get page :auto-load-paused-p)))
              (with-current-buffer buffer
                (funcall
                 (appkit-scroll-observer-end-function observer)
                 'window 975 1000)
                (should-not
                 (string-match-p "Unable to load" (buffer-string))))
              (should (= 2 (length requests)))
              (should-not (member "Older page failed" messages)))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-tweet-mutations-keep-inactive-feed-coherent
    ()
  "Tweet updates and deletion should reach both cached primary feeds."
  (let ((chirp--app nil) buffer callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (callback &rest _args)
                  (setq callbacks (append callbacks (list callback)))))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (cl-letf (((symbol-function 'chirp-backend-request)
                       (lambda (_args success &optional _error)
                         (funcall success nil nil)))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (setq buffer (chirp-timeline-open-home))
            (let
                ((view
                  (with-current-buffer buffer
                    (appkit-current-surface))))
              (funcall (nth 0 callbacks)
                       (list
                        '(:kind tweet :id "1" :text "Home" :liked-p
                          nil))
                       nil)
              (appkit-loop-run-pass (appkit-surface-loop view))
              (with-current-buffer buffer
                (chirp-toggle-home-following))
              (funcall (nth 1 callbacks)
                       (list
                        '(:kind tweet :id "1" :text "Following"
                          :liked-p nil))
                       nil)
              (appkit-loop-run-pass (appkit-surface-loop view))
              (with-current-buffer buffer
                (chirp-toggle-home-following))
              (appkit-loop-run-pass (appkit-surface-loop view))
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "Home")
                (chirp-like-at-point))
              (appkit-loop-run-pass (appkit-surface-loop view))
              (let
                  ((states
                    (chirp--session-primary-feed-states
                     (chirp--session))))
                (should
                 (plist-get
                  (car (plist-get (gethash 'home states) :items))
                  :liked-p))
                (should
                 (plist-get
                  (car (plist-get (gethash 'following states) :items))
                  :liked-p))
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (search-forward "Home")
                  (chirp-delete-at-point))
                (appkit-loop-run-pass (appkit-surface-loop view))
                (should-not (plist-get (gethash 'home states) :items))
                (should-not
                 (plist-get (gethash 'following states) :items)))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-quoted-action-invalidates-owning-row ()
  "Updating a quoted tweet should target its canonical top-level owner row."
  (let ((chirp--app nil) buffer callback)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success &rest _args) (setq callback success)))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let*
                ((view
                  (with-current-buffer buffer
                    (appkit-current-surface)))
                 (quoted
                  '(:kind tweet :id "quoted" :text "Quoted" :liked-p
                    nil))
                 (outer
                  (list :kind 'tweet :id "outer" :text "Outer"
                        :quoted-tweet quoted)))
              (funcall callback (list outer) nil)
              (appkit-loop-run-pass (appkit-surface-loop view))
              (cl-letf (((symbol-function 'chirp-backend-request)
                         (lambda (_args success &optional _error)
                           (funcall success nil nil))))
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (search-forward "Quoted")
                  (chirp-like-at-point)))
              (appkit-loop-run-pass (appkit-surface-loop view))
              (should (plist-get quoted :liked-p)))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest
    chirp-primary-media-completion-invalidates-only-dependent-row ()
  "A media resource completion should reprint only its dependent tweet row."
  (let ((chirp--app nil) buffer callback printed)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success &rest _args) (setq callback success)))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let
                ((view
                  (with-current-buffer buffer
                    (appkit-current-surface))))
              (funcall callback
                       (list
                        '(:kind tweet :id "1" :text "First"
                          :author-avatar-url
                          "https://example.com/one.jpg")
                        '(:kind tweet :id "2" :text "Second"
                          :author-avatar-url
                          "https://example.com/two.jpg"))
                       nil)
              (appkit-loop-run-pass (appkit-surface-loop view))
              (let
                  ((printer
                    (symbol-function 'chirp-render-print-tweet-row)))
                (cl-letf
                    (((symbol-function 'chirp-render-print-tweet-row)
                      (lambda (row)
                        (push (appkit-projection-row-key row) printed)
                        (funcall printer row))))
                  (chirp-media--invalidate-resource
                   "https://example.com/one.jpg")

                  (appkit-loop-run-pass (appkit-surface-loop view))))
              (should (equal printed '((tweet "1")))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-stop-makes-late-primary-callback-inert ()
  "A callback arriving after stop should not mutate or recreate the session."
  (let ((chirp--app nil) buffer callback view state)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success &rest _args) (setq callback success)))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home) view
                  (with-current-buffer buffer
                    (appkit-current-surface))
                  state (appkit-surface-model view))
            (chirp-stop) (should-not (appkit-surface-live-p view))
            (funcall callback
                     (list '(:kind tweet :id "late" :text "late")) nil)
            (should-not chirp--app)
            (should-not (plist-get state :items))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-quit-current-buffer-keeps-main-timeline-buffers ()
  "Quitting For You/Following should keep the timeline buffer alive."
  (let ((previous (generate-new-buffer "*chirp-prev-test*"))
        (timeline (generate-new-buffer " *chirp-home*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer previous)
          (switch-to-buffer timeline)
          (with-current-buffer timeline
            (chirp-view-mode)
            (setq-local chirp--timeline-kind 'home))
          (chirp-quit-current-buffer)
          (should (eq (window-buffer (selected-window)) previous))
          (should (buffer-live-p timeline)))
      (dolist (buffer (list previous timeline))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-quit-current-buffer-kills-secondary-chirp-views ()
  "Quitting secondary Chirp views should still kill the current buffer."
  (let ((previous (generate-new-buffer "*chirp-prev-test*"))
        (detail (generate-new-buffer " *chirp-detail*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer previous)
          (switch-to-buffer detail)
          (with-current-buffer detail
            (chirp-view-mode)
            (setq-local chirp--timeline-kind nil)
            (setq-local chirp--view-title "Thread"))
          (chirp-quit-current-buffer)
          (should (eq (window-buffer (selected-window)) previous))
          (should-not (buffer-live-p detail)))
      (dolist (buffer (list previous detail))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest chirp-tweet-from-x-keeps-session-overrides-explicit ()
  "X payload conversion should stay pure until state overrides are applied."
  (unwind-protect
      (progn
        (chirp-set-tweet-state-override "1" :liked-p t)
        (let ((tweet
               (chirp--tweet-from-x
                '((id . "1")
                  (text . "hello")
                  (liked . chirp-json-false)
                  (author . ((screenName . "alice")))))))
          (should-not (plist-get tweet :liked-p))
          (should (plist-get
                   (chirp-apply-tweet-state-overrides tweet)
                   :liked-p))))
    (chirp-clear-tweet-state-overrides "1")))

(ert-deftest chirp-top-level-tweets-from-x-hide-promoted-posts ()
  "Promoted tweets should be dropped when filtering is enabled."
  (let ((chirp-hide-promoted-posts t))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (chirp--top-level-tweets-from-x
                            (list '((id . "1")
                                    (text . "normal")
                                    (author . ((screenName . "alice")
                                                 (name . "Alice"))))
                                  '((id . "2")
                                    (text . "ad")
                                    (isPromoted . t)
                                    (author . ((screenName . "brand")
                                                 (name . "Brand")))))))
                   '("1")))))

(ert-deftest chirp-top-level-tweets-from-x-keep-retweet-identity ()
  "Self-retweets and their originals should remain distinct timeline entries."
  (let* ((retweeter
          '((rest_id . "10")
            (legacy . ((screen_name . "alice")
                         (name . "Alice")))))
         (author
          '((rest_id . "20")
            (legacy . ((screen_name . "bob")
                         (name . "Bob")))))
         (original
          `((rest_id . "200")
            (core . ((user_results . ((result . ,author)))))
            (legacy . ((full_text . "Original post")))))
         (retweet
          `((rest_id . "100")
            (core . ((user_results . ((result . ,retweeter)))))
            (legacy . ((full_text . "RT @bob: Original post")
             (retweeted_status_result . ((result . ,original)))))))
         (tweets (chirp--top-level-tweets-from-x
                  (list retweet original))))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           tweets)
                   '("200" "200")))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :retweet-id))
                           tweets)
                   '("100" nil)))
    (should (equal (mapcar #'appkit-projection-row-key
                           (chirp-render-project-tweet-rows tweets))
                   '((tweet "100") (tweet "200"))))))

(ert-deftest chirp-timeline-projection-keys-distinct-tweet-occurrences ()
  "One tweet in two X entries should remain two independently keyed rows."
  (let* ((tweet-id "2094737512484970656")
         (tweets
          (list
           (list :kind 'tweet :id tweet-id
                 :timeline-entry-id "tweet-2094737512484970656")
           (list
            :kind 'tweet :id tweet-id
            :timeline-entry-id
            "profile-conversation-2094846962946801665-tweet-2094737512484970656")))
         (rows (chirp-render-project-tweet-rows tweets)))
    (should
     (equal (mapcar #'appkit-projection-row-key rows)
            `((tweet ,tweet-id "tweet-2094737512484970656")
              (tweet
               ,tweet-id
               "profile-conversation-2094846962946801665-tweet-2094737512484970656"))))
    (dolist (row rows)
      (should (member (list 'tweet tweet-id)
                      (appkit-projection-row-dependencies row))))))

(ert-deftest chirp-tweet-rerender-updates-all-timeline-occurrences ()
  "A tweet update should redraw every distinct occurrence of that tweet."
  (let
      ((chirp--app nil) (chirp-rerender-idle-delay 0) (tweet-id "2094737512484970656")
       buffer callback view)
    (unwind-protect
        (cl-letf
            (((symbol-function 'chirp-backend-feed)
              (lambda (success &rest _args) (setq callback success)))
             ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
             ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (setq buffer (chirp-timeline-open-home))
          (with-current-buffer buffer
            (setq view (appkit-current-surface)))
          (funcall callback
                   (list
                    (list :kind 'tweet :id tweet-id :text "Before"
                          :timeline-entry-id
                          "tweet-2094737512484970656")
                    (list :kind 'tweet :id tweet-id :text "Before"
                          :timeline-entry-id
                          "profile-conversation-1-tweet-2094737512484970656"))
                   nil)
          (while (> (appkit-loop-pending-count (appkit-surface-loop view)) 0)
            (appkit-loop-run-pass (appkit-surface-loop view)))
          (with-current-buffer buffer
            (should (= (how-many "Before" (point-min) (point-max)) 2)))
          (dolist (tweet (plist-get (appkit-surface-model view) :items))
            (setf (plist-get tweet :text) "After"))
          (chirp-request-tweet-rerender tweet-id buffer)
          (while (> (appkit-loop-pending-count (appkit-surface-loop view)) 0)
            (appkit-loop-run-pass (appkit-surface-loop view)))
          (with-current-buffer buffer
            (should (= (how-many "After" (point-min) (point-max)) 2))
            (should-not (string-search "Before" (buffer-string)))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-top-level-tweets-from-x-can-keep-promoted-posts ()
  "Promoted tweets should remain visible when filtering is disabled."
  (let ((chirp-hide-promoted-posts nil))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                           (chirp--top-level-tweets-from-x
                            (list '((id . "1")
                                    (text . "normal")
                                    (author . ((screenName . "alice")
                                                 (name . "Alice"))))
                                  '((id . "2")
                                    (text . "ad")
                                    (isPromoted . t)
                                    (author . ((screenName . "brand")
                                                 (name . "Brand")))))))
                   '("1" "2")))))

(ert-deftest chirp-status-appears-in-mode-line ()
  "Persistent Chirp status should stay visible in the mode line."
  (with-temp-buffer
    (chirp-view-mode)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest _args)
                 'chirp-test-timer))
              ((symbol-function 'timerp)
               (lambda (value)
                 (eq value 'chirp-test-timer)))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'float-time)
               (lambda (&optional _time)
                 100.0)))
      (chirp-set-status (current-buffer) "Loading thread...")
      (setq-local chirp--status-start-time 97.5)
      (let ((rendered (chirp--mode-line-status-string)))
        (should (string-match-p "Loading thread\\.\\.\\." rendered))
        (should (string-match-p "2\\.5s" rendered)))
      (chirp-clear-status (current-buffer))
      (should-not (chirp--mode-line-status-string)))))

(ert-deftest
    chirp-timeline-open-likes-resolves-current-user-before-fetching
    ()
  "Liked view should resolve the current handle before fetching likes."
  (let (whoami-called likes-handle installed)
    (unwind-protect
        (cl-letf
            (((symbol-function 'chirp-begin-background-request)
              (lambda (_buffer _title) 'token))
             ((symbol-function 'chirp-request-current-p)
              (lambda (_buffer _token) t))
             ((symbol-function 'chirp-backend-whoami)
              (lambda (callback &optional _errback)
                (setq whoami-called t)
                (funcall callback '(:handle "alice") nil)))
             ((symbol-function 'chirp-backend-likes)
              (lambda (handle callback &optional _errback)
                (setq likes-handle handle)
                (funcall callback (list (list :id "1")) nil)))
             ((symbol-function 'chirp-timeline--install-tweets)
              (lambda (view tweets)
                (setq installed
                      (list
                       (plist-get (appkit-surface-model view) :title)
                       tweets)))))
          (chirp-timeline-open-likes) (should whoami-called)
          (should (equal likes-handle "alice"))
          (should (equal installed '("Liked: @alice" ((:id "1"))))))
      (chirp-stop))))

(ert-deftest chirp-timeline-open-list-prompts-from-accessible-lists
    ()
  "List selection should prompt with accessible lists and open the chosen id."
  (let (captured-target installed seen-prompt seen-collection)
    (unwind-protect
        (cl-letf
            (((symbol-function 'chirp-backend-lists)
              (lambda (callback &optional _errback)
                (funcall callback
                         '(((id . "1956792682412345678")
                            (name . "Emacs") (mode . "private")
                            (sources "owned")
                            (owner (screenName . "lucius"))))
                         nil)))
             ((symbol-function 'completing-read)
              (lambda (prompt collection &rest _args)
                (setq seen-prompt prompt seen-collection collection)
                (caar collection)))
             ((symbol-function 'chirp-begin-background-request)
              (lambda (_buffer _title) 'token))
             ((symbol-function 'chirp-request-current-p)
              (lambda (_buffer _token) t))
             ((symbol-function 'chirp-backend-list)
              (lambda (list-target callback &optional _errback)
                (setq captured-target list-target)
                (funcall callback (list (list :id "1")) nil)))
             ((symbol-function 'chirp-timeline--install-tweets)
              (lambda (view tweets)
                (setq installed
                      (list
                       (plist-get (appkit-surface-model view) :title)
                       tweets)))))
          (chirp-timeline-open-list)
          (should (equal seen-prompt "List (1): "))
          (should (string-match-p "@lucius" (caar seen-collection)))
          (should (string-match-p "owned" (caar seen-collection)))
          (should (equal captured-target "1956792682412345678"))
          (should
           (equal installed '("List: 1956792682412345678" ((:id "1"))))))
      (chirp-stop))))

(ert-deftest chirp-timeline-list-picker-ignores-a-dead-target-buffer ()
  "Delayed list discovery should not prompt after its target buffer dies."
  (let (buffer callback prompted)
    (cl-letf (((symbol-function 'chirp-backend-lists)
               (lambda (success &optional _errback)
                 (setq callback success)))
              ((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (setq prompted t))))
      (setq buffer (chirp-timeline-open-list))
      (kill-buffer buffer)
      (funcall callback
               '(((id . "1") (name . "One")))
               nil))
    (should-not prompted)))

(ert-deftest chirp-timeline-open-list-uses-list-title-and-renderer ()
  "List view should fetch tweets and render under a list-specific title."
  (let (captured-target installed)
    (unwind-protect
        (cl-letf
            (((symbol-function 'chirp-begin-background-request)
              (lambda (_buffer _title) 'token))
             ((symbol-function 'chirp-request-current-p)
              (lambda (_buffer _token) t))
             ((symbol-function 'chirp-backend-list)
              (lambda (list-target callback &optional _errback)
                (setq captured-target list-target)
                (funcall callback (list (list :id "1")) nil)))
             ((symbol-function 'chirp-timeline--install-tweets)
              (lambda (view tweets)
                (setq installed
                      (list
                       (plist-get (appkit-surface-model view) :title)
                       tweets)))))
          (chirp-timeline-open-list "1956792682412345678")
          (should (equal captured-target "1956792682412345678"))
          (should
           (equal installed '("List: 1956792682412345678" ((:id "1"))))))
      (chirp-stop))))

(ert-deftest chirp-primary-timeline-sync-preserves-point-within-entry
    ()
  "Background primary timeline syncs should preserve semantic point offsets."
  (let
      ((tweet-a
        '(:kind tweet :id "1" :text "Alpha line one\nAlpha line two"))
       (tweet-b
        '(:kind tweet :id "2" :text "Beta line one\nBeta line two"))
       buffer callback view)
    (unwind-protect
        (cl-letf
            (((symbol-function 'chirp-backend-feed)
              (lambda (success &rest _args) (setq callback success)))
             ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
             ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
          (setq buffer (chirp-timeline-open-home))
          (with-current-buffer buffer
            (setq view (appkit-current-surface)))
          (funcall callback (list tweet-a tweet-b) nil)
          (appkit-loop-run-pass (appkit-surface-loop view))
          (with-current-buffer buffer
            (search-forward "Beta line two")
            (let ((before (point)))
              (should
               (equal (chirp--text-property-at-point 'chirp-entry-id)
                      '(tweet "2")))
              (chirp-request-rerender buffer 60)
              (appkit-loop-run-pass (appkit-surface-loop view))
              (should
               (equal (plist-get (chirp-entry-at-point) :id) "2"))
              (should (> (point) (chirp--current-entry-start)))
              (should (= before (point))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-polling-timer-is-owned-by-the-view ()
  "Killing the primary view should cancel its foreground polling timer."
  (let
      ((chirp--app nil) (chirp-timeline-poll-interval 30) buffer
       canceled scheduled)
    (unwind-protect
        (cl-letf
            (((symbol-function 'run-at-time)
              (lambda (delay repeat function &rest arguments)
                (if (= delay 30)
                    (progn
                      (setq scheduled
                            (list delay repeat function arguments))
                      'chirp-test-poll-timer)
                  'chirp-test-sync-timer)))
             ((symbol-function 'run-with-idle-timer)
              (lambda (&rest _arguments) 'chirp-test-scroll-timer))
             ((symbol-function 'timerp)
              (lambda (object)
                (memq object
                      '(chirp-test-poll-timer chirp-test-sync-timer
                        chirp-test-scroll-timer))))
             ((symbol-function 'cancel-timer)
              (lambda (timer) (push timer canceled)))
             ((symbol-function 'chirp-backend-feed)
              (lambda (&rest _arguments) 'request)))
          (setq buffer (chirp-timeline-open-home))
          (let
              ((view
                (with-current-buffer buffer (appkit-current-surface))))
            (should
             (equal (butlast scheduled) '(30 30 chirp-timeline--poll)))
            (should (equal (car (last scheduled)) (list view)))
            (should
             (cl-find 'chirp-test-poll-timer
                      (appkit-surface-handles view) :key
                      #'appkit-handle-object))
            (kill-buffer buffer) (setq buffer nil)
            (should (memq 'chirp-test-poll-timer canceled))
            (should-not (appkit-surface-live-p view))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-poll-stages-new-posts-until-activated ()
  "Foreground polling should expose a header button without moving the row."
  (let
      ((chirp--app nil) (chirp-timeline-poll-interval nil) buffer
       callbacks)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success &rest _args)
                  (setq callbacks (append callbacks (list success)))
                  (list 'request (length callbacks))))
               ((symbol-function 'chirp-media-prefetch-tweets)
                #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let
                ((view
                  (with-current-buffer buffer
                    (appkit-current-surface))))
              (funcall (nth 0 callbacks)
                       (list
                        '(:kind tweet :id "old" :text
                          "Visible old post"))
                       nil)
              (appkit-loop-run-pass (appkit-surface-loop view))
              (with-current-buffer buffer
                (search-forward "Visible old post"))
              (chirp-timeline--poll view)
              (should (= (length callbacks) 2))
              (funcall (nth 1 callbacks)
                       (list
                        '(:kind tweet :id "new" :text
                          "Pending new post")
                        '(:kind tweet :id "old" :text
                          "Visible old post"))
                       nil)
              (appkit-loop-run-pass (appkit-surface-loop view))
              (let ((state (appkit-surface-model view)))
                (should
                 (equal
                  (mapcar (lambda (tweet) (plist-get tweet :id))
                          (plist-get state :items))
                  '("old")))
                (should
                 (equal
                  (mapcar (lambda (tweet) (plist-get tweet :id))
                          (plist-get state :pending-new-items))
                  '("new"))))
              (with-current-buffer buffer
                (should
                 (equal (plist-get (chirp-entry-at-point) :id) "old"))
                (should header-line-format)
                (should-not
                 (string-match-p "Show 1 post" (buffer-string)))
                (let*
                    ((header (chirp-timeline--header-line))
                     (start (string-match "Show 1 post" header))
                     (map
                      (and start
                           (get-text-property start 'keymap header)))
                     (command
                      (and map (lookup-key map [header-line mouse-1]))))
                  (should start)
                  (should (eq command #'chirp-timeline-show-new))
                  (should
                   (eq (lookup-key map [header-line down-mouse-1])
                       #'ignore))
                  (funcall-interactively command)))
              (appkit-loop-run-pass (appkit-surface-loop view))
              (let ((state (appkit-surface-model view)))
                (should-not (plist-get state :pending-new-items))
                (should
                 (equal
                  (mapcar (lambda (tweet) (plist-get tweet :id))
                          (plist-get state :items))
                  '("new" "old"))))
              (with-current-buffer buffer
                (should-not header-line-format)
                (should
                 (equal (plist-get (chirp-entry-at-point) :id) "new"))
                (should-not
                 (string-match-p "Show 1 post" (buffer-string)))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-timeline-staging-accumulates-unique-new-posts ()
  "Successive foreground polls should retain unseen posts in newest-first order."
  (let* ((current (list '(:kind tweet :id "old")))
         (pending (list '(:kind tweet :id "one" :text "older copy")))
         (fetched (list '(:kind tweet :id "two")
                        '(:kind tweet :id "one" :text "fresh copy")
                        '(:kind tweet :id "old")))
         (staged
          (chirp-timeline--stage-new-tweets current pending fetched)))
    (should
     (equal (mapcar (lambda (tweet) (plist-get tweet :id)) staged)
            '("two" "one")))
    (should (equal (plist-get (cadr staged) :text) "fresh copy"))))

(ert-deftest chirp-timeline-refresh-uses-smaller-head-window ()
  "Refreshing should fetch a smaller head page when configured."
  (let ((state (chirp-timeline--make-state 'home 20))
        (chirp-timeline-refresh-max-results 10))
    (should (= (chirp-timeline--fetch-count state 'refresh) 10))))

(ert-deftest chirp-timeline-refresh-can-use-current-limit ()
  "Refreshing should keep the old fetch size when the head-window override is disabled."
  (let ((state (chirp-timeline--make-state 'home 20))
        (chirp-timeline-refresh-max-results nil))
    (should (= (chirp-timeline--fetch-count state 'refresh) 20))))

(ert-deftest chirp-window-state-restore-preserves-point-and-scroll ()
  "Window-state helpers should preserve point and scroll position."
  (let ((buffer (generate-new-buffer " *chirp-window-state*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (chirp-view-mode)
            (let ((inhibit-read-only t))
              (dotimes (index 80)
                (insert (format "line %02d\n" index))))
            (goto-char (point-min))
            (forward-line 40)
            (set-window-start (selected-window) (line-beginning-position))
            (set-window-vscroll (selected-window) 8 t)
            (let ((state (chirp-capture-window-state buffer))
                  (point (point))
                  (window-start (window-start (selected-window)))
                  (vscroll (window-vscroll (selected-window) t)))
              (goto-char (point-min))
              (set-window-start (selected-window) (point-min))
              (set-window-vscroll (selected-window) 0 t)
              (chirp-restore-window-state state)
              (should (= (point) point))
              (should (= (window-start (selected-window)) window-start))
              (should (= (window-vscroll (selected-window) t) vscroll)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-clean-text-decodes-html-entities ()
  "Tweet text should decode common HTML entities."
  (should (equal (chirp-clean-text "a &gt; b &amp; c &lt; d")
                 "a > b & c < d"))
  (should (equal (chirp-clean-text "say &quot;hi&quot; &#39;now&#39;")
                 "say \"hi\" 'now'"))
  (should (equal (chirp-clean-text "A&#10;B&#x21;")
                 "A\nB!")))

(ert-deftest chirp-primary-folds-refreshes-and-revokes-before-cancel ()
  "A refresh burst starts only its final request; cancel callbacks are inert."
  (let ((chirp--app nil) (chirp-timeline-poll-interval nil)
        (chirp-timeline-auto-load-threshold nil) buffer requests canceled)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success following failure count cursor owner)
                  (let ((request (list :success success :failure failure
                                       :following following :count count
                                       :cursor cursor :owner owner
                                       :buffer (generate-new-buffer " *chirp-feed*"))))
                    (setq requests (append requests (list request)))
                    (plist-get request :buffer))))
               ((symbol-function 'chirp-x-cancel-request)
                (lambda (buffer)
                  (let ((request (cl-find buffer requests
                                          :key (lambda (item) (plist-get item :buffer)))))
                    (push buffer canceled)
                    (funcall (plist-get request :failure) "Canceled old request")
                    (funcall (plist-get request :success)
                             (list (list :kind 'tweet :id "canceled" :text "Canceled")) nil)
                    (kill-buffer buffer))))
               ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let* ((view (with-current-buffer buffer (appkit-current-surface)))
                   (state (appkit-surface-model view)))
              (dotimes (_ 3)
                (appkit-surface-post
                 view (list 'chirp-timeline 'request state view 'refresh nil)))
              (chirp-timeline-test--drain view)
              (should (= (length requests) 2))
              (should (eq (plist-get (cadr requests) :owner) view))
              (should (equal canceled (list (plist-get (car requests) :buffer))))
              (funcall (plist-get (cadr requests) :success)
                       (list (list :kind 'tweet :id "current" :text "Current")) nil)
              (chirp-timeline-test--drain view)
              (funcall (plist-get (car requests) :failure) "Late failure")
              (funcall (plist-get (car requests) :success)
                       (list (list :kind 'tweet :id "late" :text "Late")) nil)
              (chirp-timeline-test--drain view)
              (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                                    (plist-get state :items))
                             '("current")))
              (should (eq (plist-get (plist-get state :status) :phase) 'idle))
              (with-current-buffer buffer
                (should (string-search "Current" (buffer-string)))
                (should-not (string-search "Canceled" (buffer-string)))))))
      (chirp-stop)
      (dolist (target (cons buffer (mapcar (lambda (item) (plist-get item :buffer))
                                         requests)))
        (when (buffer-live-p target) (kill-buffer target))))))

(ert-deftest chirp-primary-synchronous-result-retires-transport-authority ()
  "An inline result remains one-shot and is not canceled by the next refresh."
  (let ((chirp--app nil) (chirp-timeline-poll-interval nil)
        (chirp-timeline-auto-load-threshold nil) buffer requests canceled)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success _following failure _count _cursor _owner)
                  (let ((request (generate-new-buffer " *chirp-inline-feed*")))
                    (push (list :buffer request :success success :failure failure) requests)
                    (when (= (length requests) 1)
                      (funcall success
                               (list (list :kind 'tweet :id "one" :text "Inline result"))
                               nil))
                    request)))
               ((symbol-function 'chirp-x-cancel-request)
                (lambda (request) (push request canceled) (kill-buffer request)))
               ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let* ((view (with-current-buffer buffer (appkit-current-surface)))
                   (first (car requests)))
              (chirp-timeline-test--drain view)
              (with-current-buffer buffer
                (should (string-search "Inline result" (buffer-string)))
                (chirp-refresh))
              (should-not canceled)
              (funcall (plist-get first :failure) "Duplicate failure")
              (funcall (plist-get first :success)
                       (list (list :kind 'tweet :id "duplicate" :text "Duplicate")) nil)
              (funcall (plist-get (car requests) :failure) "Current failure")
              (chirp-timeline-test--drain view)
              (with-current-buffer buffer
                (should (string-search "Inline result" (buffer-string)))
                (should (string-search "Current failure" (buffer-string)))
                (should-not (string-search "Duplicate" (buffer-string)))))))
      (chirp-stop)
      (dolist (target (cons buffer (mapcar (lambda (item) (plist-get item :buffer))
                                         requests)))
        (when (buffer-live-p target) (kill-buffer target))))))

(ert-deftest chirp-primary-switch-rejects-results-from-previous-visit ()
  "Switching away and back cannot restore a previous request's authority."
  (let ((chirp--app nil) (chirp-timeline-poll-interval nil)
        (chirp-timeline-auto-load-threshold nil) buffer requests)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success following failure &rest _args)
                  (setq requests
                        (append requests (list (list success failure following)))) nil))
               ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-surface))))
              (chirp-timeline-open-following)
              (chirp-timeline-open-home)
              (should (= (length requests) 3))
              (should (nth 2 (nth 1 requests)))
              (funcall (nth 0 (nth 0 requests))
                       (list (list :kind 'tweet :id "old-home" :text "Old Home")) nil)
              (funcall (nth 1 (nth 1 requests)) "Old Following error")
              (funcall (nth 0 (nth 2 requests))
                       (list (list :kind 'tweet :id "home" :text "Current Home")) nil)
              (chirp-timeline-test--drain view)
              (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                                    (plist-get (appkit-surface-model view) :items))
                             '("home")))
              (with-current-buffer buffer
                (should (string-search "Current Home" (buffer-string)))
                (should-not (string-search "Old" (buffer-string)))))))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-refresh-retains-tail-pending-and-reading-position ()
  "Refresh preserves an extended feed's cursor and point across feed switches."
  (let ((chirp--app nil) (chirp-default-max-results 2)
        (chirp-timeline-poll-interval nil) (chirp-timeline-auto-load-threshold nil)
        buffer requests)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success &rest _args)
                  (setq requests (append requests (list success))) nil))
               ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-surface))))
              (funcall (nth 0 requests)
                       (list (list :kind 'tweet :id "one" :text "First")
                             (list :kind 'tweet :id "two" :text "Second\nreading here"))
                       '((pagination (nextCursor . "page-2"))))
              (chirp-timeline-test--drain view)
              (with-current-buffer buffer (chirp-load-more))
              (funcall (nth 1 requests)
                       (list (list :kind 'tweet :id "two" :text "Duplicate")
                             (list :kind 'tweet :id "three" :text "Older"))
                       '((pagination (nextCursor . "page-3"))))
              (chirp-timeline-test--drain view)
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "reading here"))
              (let ((position (with-current-buffer buffer (point)))
                    (home (appkit-surface-model view)))
                (with-current-buffer buffer (chirp-refresh))
                (funcall (nth 2 requests)
                         (list (list :kind 'tweet :id "new" :text "Pending new")
                               (list :kind 'tweet :id "one" :text "First"))
                         '((pagination (nextCursor . "head-cursor"))))
                (chirp-timeline-test--drain view)
                (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                                      (plist-get home :items))
                               '("one" "two" "three")))
                (should (equal (plist-get (plist-get home :page) :next-cursor)
                               "page-3"))
                (with-current-buffer buffer (should (= (point) position)))
                (chirp-timeline-open-following)
                (funcall (nth 3 requests)
                         (list (list :kind 'tweet :id "following" :text "Following")) nil)
                (chirp-timeline-test--drain view)
                (chirp-timeline-open-home)
                (should (= (length requests) 4))
                (should (eq home (appkit-surface-model view)))
                (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                                      (plist-get home :pending-new-items))
                               '("new")))
                (with-current-buffer buffer
                  (should (= (point) position))
                  (should header-line-format)
                  (chirp-timeline-show-new)
                  (should (equal (plist-get (chirp-entry-at-point) :id) "new")))))))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-older-stops-on-missing-or-unchanged-cursor ()
  "An older page deduplicates rows and cannot repeatedly fetch a stuck cursor."
  (dolist (cursor '(nil "older"))
    (let ((chirp--app nil) (chirp-timeline-poll-interval nil)
          (chirp-timeline-auto-load-threshold nil) buffer requests)
      (unwind-protect
          (save-window-excursion
            (cl-letf
                (((symbol-function 'chirp-backend-feed)
                  (lambda (success &rest _args)
                    (setq requests (append requests (list success))) nil))
                 ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                 ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
              (setq buffer (chirp-timeline-open-home))
              (let ((view (with-current-buffer buffer (appkit-current-surface))))
                (funcall (car requests)
                         (list (list :kind 'tweet :id "one" :text "First"))
                         '((pagination (nextCursor . "older"))))
                (chirp-timeline-test--drain view)
                (with-current-buffer buffer (chirp-load-more))
                (funcall (cadr requests)
                         (list (list :kind 'tweet :id "one" :text "Duplicate")
                               (list :kind 'tweet :id "two" :text "Second"))
                         (and cursor `((pagination (nextCursor . ,cursor)))))
                (chirp-timeline-test--drain view)
                (with-current-buffer buffer (chirp-load-more))
                (should (= (length requests) 2))
                (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                                      (plist-get (appkit-surface-model view) :items))
                               '("one" "two"))))))
        (chirp-stop)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest chirp-primary-poll-errors-preserve-the-reader ()
  "Polling failures are silent and retain the visible row and its offset."
  (let ((chirp--app nil) (chirp-timeline-poll-interval nil)
        (chirp-timeline-auto-load-threshold nil) buffer success failure messages)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (resolve _following reject &rest _args)
                  (setq success resolve failure reject) nil))
               ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
               ((symbol-function 'message)
                (lambda (format-string &rest args)
                  (when format-string (push (apply #'format format-string args) messages)))))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-surface))))
              (funcall success (list (list :kind 'tweet :id "one" :text "Keep\nreading")) nil)
              (chirp-timeline-test--drain view)
              (with-current-buffer buffer (search-forward "reading"))
              (let ((point (with-current-buffer buffer (point))))
                (chirp-timeline--poll view)
                (funcall failure "Quiet polling failure")
                (chirp-timeline-test--drain view)
                (should-not messages)
                (with-current-buffer buffer
                  (should (= (point) point))
                  (should (string-search "reading" (buffer-string)))
                  (should-not (string-search "failure" (buffer-string))))))))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-assets-use-exact-owner-and-synchronous-quote-cache ()
  "Post-commit assets can use a synchronous quote cache without active-pass I/O."
  (let ((chirp--app nil) (chirp-timeline-poll-interval nil)
        (chirp-timeline-auto-load-threshold nil) buffer success deferred prefetched
        (schedule (symbol-function 'run-at-time))
        (full (list :kind 'tweet :id "quote" :text "Full cached quote"
                    :chirp-enriched-p t)))
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'run-at-time)
                (lambda (delay repeat function &rest args)
                  (if (and (equal delay 0) (null args))
                      (progn (push (cons function args) deferred) (timer-create))
                    (apply schedule delay repeat function args))))
               ((symbol-function 'chirp-backend-feed)
                (lambda (resolve &rest _args) (setq success resolve) nil))
               ((symbol-function 'chirp-media-prefetch-tweets)
                (lambda (tweets target)
                  (push (list chirp--app target tweets) prefetched)))
               ((symbol-function 'chirp--request-quoted-tweet)
                (lambda (_id callback) (funcall callback full))))
            (setq buffer (chirp-timeline-open-home))
            (let* ((view (with-current-buffer buffer (appkit-current-surface)))
                   (app (appkit-surface-app view))
                   (tweet (list :kind 'tweet :id "outer" :text "Outer"
                                :quoted-tweet (list :kind 'tweet :id "quote" :text "Preview"))))
              (funcall success (list tweet) nil)
              (chirp-timeline-test--drain view)
              ;; Run the fixed asset continuation from an unrelated buffer and
              ;; with no global current App; only its captured owner is valid.
              (let ((job (pop deferred)) (chirp--app nil))
                (with-temp-buffer (apply (car job) (cdr job))))
              (chirp-timeline-test--drain view)
              (should (eq (caar prefetched) app))
              (should (eq (cadar prefetched) buffer))
              (should (eq (plist-get tweet :quoted-tweet) full))
              (with-current-buffer buffer
                (should (string-search "Full cached quote" (buffer-string)))
                (chirp-refresh))
              (funcall success (list (list :kind 'tweet :id "new" :text "Pending")) nil)
              (chirp-timeline-test--drain view)
              (let ((job (pop deferred)))
                (chirp-timeline-open-following)
                (apply (car job) (cdr job)))
              (should (= (length prefetched) 1))
              (funcall success
                       (list (list :kind 'tweet :id "following" :text "Following")) nil)
              (chirp-timeline-test--drain view)
              (let ((job (pop deferred)))
                (kill-buffer buffer)
                (let ((chirp--app nil))
                  (with-temp-buffer (apply (car job) (cdr job)))
                  (should-not chirp--app)))
              (should (= (length prefetched) 1))
              (funcall success (list (list :kind 'tweet :id "late")) nil)
              (should-not (appkit-surface-live-p view)))))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-primary-synchronous-failure-remains-retryable ()
  "An inline credential/transport failure can be refreshed normally."
  (let ((chirp--app nil) (chirp-timeline-poll-interval nil)
        (chirp-timeline-auto-load-threshold nil) buffer (attempts 0))
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'chirp-backend-feed)
                (lambda (success _following failure &rest _args)
                  (if (= (cl-incf attempts) 1)
                      (funcall failure "Inline failure")
                    (funcall success
                             (list (list :kind 'tweet :id "retry" :text "Retry succeeded"))
                             nil))
                  nil))
               ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
               ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore))
            (setq buffer (chirp-timeline-open-home))
            (let ((view (with-current-buffer buffer (appkit-current-surface))))
              (chirp-timeline-test--drain view)
              (with-current-buffer buffer
                (should (string-search "Inline failure" (buffer-string)))
                (chirp-refresh))
              (chirp-timeline-test--drain view)
              (should (= attempts 2))
              (with-current-buffer buffer
                (should (string-search "Retry succeeded" (buffer-string)))
                (should-not (string-search "Inline failure" (buffer-string)))))))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(provide 'chirp-timeline-test)

;;; chirp-timeline-test.el ends here
