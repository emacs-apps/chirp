;;; chirp-thread-test.el --- Tests for Chirp thread loading -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'face-remap)
(require 'time-date)
(require 'chirp-thread)

(defun chirp-thread-test--render-view
    (scratch title refresh tweets &optional anchor-id display-p
             focus-id)
  "Render TWEETS through the production thread view for one test SCRATCH."
  (let*
      ((view
        (chirp-thread--ensure-view title refresh focus-id
                                   (list 'thread 'test
                                         (buffer-name scratch))))
       (position
        (or (and (stringp anchor-id) (list 'tweet anchor-id))
            (and (stringp focus-id) (list 'tweet focus-id)) 'first)))
    (chirp-thread--present view tweets position)
    (while (> (appkit-loop-pending-count (appkit-surface-loop view)) 0)
      (appkit-loop-run-pass (appkit-surface-loop view)))
    (when display-p
      (chirp-display-buffer (appkit-surface-buffer view)))
    (appkit-surface-buffer view)))

(ert-deftest chirp-thread-open-tweet-renders-seed-before-network-thread-load ()
  "Opening a thread from a visible tweet should render that tweet immediately."
  (let ((buffer (generate-new-buffer " *chirp-thread-seed-test*"))
        thread-callback
        renders)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'thread-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'thread-token)))
                  ((symbol-function 'chirp-backend-thread)
                   (lambda (_target callback &optional _errback)
                     (setq thread-callback callback)))
                  ((symbol-function 'chirp-backend-article) #'ignore)
                  ((symbol-function 'chirp-thread--present)
                   (lambda (_view ordered &optional _position)
                     (push ordered renders)))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
                  ((symbol-function 'chirp-display-buffer) #'ignore))
          (chirp-thread-open-tweet
           '(:kind tweet
             :id "123"
             :text "Focus tweet"))
          (should (functionp thread-callback))
          (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                                 (car (last renders)))
                         '("123")))
          (funcall thread-callback
                   (list '(:kind tweet :id "123" :text "Focus tweet")
                         '(:kind tweet :id "456" :text "Reply"))
                   nil)
          (should (equal (mapcar (lambda (tweet) (plist-get tweet :id))
                                 (car renders))
                         '("123" "456"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-thread-discussion-rows-compute-visible-parent-depth ()
  "Discussion rows should derive stable parent keys and visible depths."
  (let* ((tweets '((:kind tweet :id "root" :text "Root")
                   (:kind tweet :id "reply" :reply-to-id "root" :text "Reply")
                   (:kind tweet :id "nested" :reply-to-id "reply" :text "Nested")))
         (rows (chirp-thread--discussion-rows tweets)))
    (should (equal (mapcar (lambda (row) (plist-get row :key)) rows)
                   '((tweet "root") (tweet "reply") (tweet "nested"))))
    (should (equal (mapcar (lambda (row) (plist-get row :parent-key)) rows)
                   '(nil (tweet "root") (tweet "reply"))))
    (should (equal (mapcar (lambda (row) (plist-get row :depth)) rows)
                   '(0 1 2)))
    (should (plist-get (car rows) :focus-p))))

(ert-deftest chirp-thread-reorder-puts-ancestor-chain-before-focus ()
  "A reply focus should keep its ancestors above it and replies below it."
  (let* ((tweets '((:kind tweet :id "focus" :reply-to-id "parent" :text "Focus")
                   (:kind tweet :id "root" :text "Root")
                   (:kind tweet :id "reply" :reply-to-id "focus" :text "Reply")
                   (:kind tweet :id "parent" :reply-to-id "root" :text "Parent")))
         (ordered (chirp-thread--reorder tweets "focus"))
         (rows (chirp-thread--discussion-rows ordered "focus")))
    (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) ordered)
                   '("root" "parent" "focus" "reply")))
    (should (equal (mapcar (lambda (row) (plist-get row :role)) rows)
                   '(chain chain focus tree)))
    (should (equal (mapcar (lambda (row) (plist-get row :depth)) rows)
                   '(0 0 0 1)))
    (should (equal (mapcar (lambda (row) (plist-get row :connector)) rows)
                   '(continue continue end nil)))
    (should (plist-get (nth 2 rows) :focus-p))))

(ert-deftest chirp-thread-discussion-rows-flatten-orphans-and-cycles ()
  "Missing or cyclic visible parents should not create invalid nesting."
  (let* ((tweets '((:kind tweet :id "root" :text "Root")
                   (:kind tweet :id "orphan" :reply-to-id "missing")
                   (:kind tweet :id "cycle-a" :reply-to-id "cycle-b")
                   (:kind tweet :id "cycle-b" :reply-to-id "cycle-a")))
         (rows (chirp-thread--discussion-rows tweets)))
    (dolist (row (cdr rows))
      (should (= (plist-get row :depth) 0))
      (should-not (plist-get row :parent-key)))))

(ert-deftest chirp-thread-projection-preserves-discussion-properties ()
  "Thread rendering should expose Appkit and Chirp entry properties."
  (let ((chirp--app nil) (scratch (generate-new-buffer " *chirp-thread-discussion-test*"))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-media-avatar-image)
                   (lambda (&rest _args) nil)))
          (setq buffer
                (chirp-thread-test--render-view scratch
                                                "Thread"
                                                #'ignore
                                                '((:kind tweet :id "root" :text "Root"
                                                   :author-name "Alice" :author-handle "alice"
                                                   :created-at "ROOT-TIME")
                                                  (:kind tweet :id "reply" :text "Reply"
                                                   :reply-to-id "root"
                                                   :reply-to-handle "Alice"
                                                   :author-name "Bob"))
                                                nil))
          (with-current-buffer buffer
            (goto-char (point-min))
            (let ((heading-line
                   (buffer-substring (line-beginning-position)
                                     (line-end-position))))
              (should (string-match-p "Alice @alice" heading-line))
              (should (string-match-p "ROOT-TIME" heading-line))
              (should (equal ""
                             (or (get-text-property (point) 'line-prefix)
                                 ""))))
            (should (equal (appkit-discussion-key-at-point)
                           '(tweet "root")))
            (should (= (get-text-property
                        (point) appkit-discussion-depth-property)
                       0))
            (should (equal (plist-get (chirp-entry-at-point) :id)
                           "root"))
            (goto-char (appkit-discussion-next-position))
            (should (equal (appkit-discussion-key-at-point)
                           '(tweet "reply")))
            (should (equal
                     (get-text-property
                      (point) appkit-discussion-parent-key-property)
                     '(tweet "root")))
            (should (= (get-text-property
                        (point) appkit-discussion-depth-property)
                       1))
            (should (equal
                     "    "
                     (get-text-property (point) 'line-prefix)))
            (forward-line 1)
            (let ((body-line
                   (buffer-substring (line-beginning-position)
                                     (line-end-position))))
              (should (string-match-p "Reply" body-line))
              (should (= 0 (- (length body-line)
                              (length (string-trim-left body-line)))))
              (should (equal "    "
                             (get-text-property (point) 'line-prefix))))
            (goto-char (point-min))
            (chirp-next-entry)
            (should (equal (plist-get (chirp-entry-at-point) :id)
                           "reply"))
            (chirp-previous-entry)
            (should (equal (plist-get (chirp-entry-at-point) :id)
                           "root"))))
      (chirp-stop)
      (when (buffer-live-p scratch)
        (kill-buffer scratch))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-thread-focus-uses-full-time-and-replies-use-compact-time ()
  "Thread focus should show an exact time while replies stay compact."
  (let* ((chirp--app nil) (chirp-language "zh-CN")
         (now (encode-time 0 0 12 13 8 2026))
         (created-at
          (format-time-string
           "%Y-%m-%dT%H:%M:%S%z"
           (time-subtract now (seconds-to-time (* 6 3600)))))
         (scratch (generate-new-buffer " *chirp-thread-time-test*"))
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'current-time) (lambda () now))
                  ((symbol-function 'chirp-media-avatar-image)
                   (lambda (&rest _args) nil)))
          (setq buffer
                (chirp-thread-test--render-view scratch
                                                "Thread"
                                                #'ignore
                                                `((:kind tweet :id "root" :text "Root"
                                                   :author-name "Alice" :created-at ,created-at)
                                                  (:kind tweet :id "reply" :text "Reply"
                                                   :reply-to-id "root" :author-name "Bob"
                                                   :created-at ,created-at))
                                                nil nil "root"))
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward
                     "上午6:00 · 2026年8月13日" nil t))
            (should (search-forward "6小时" nil t))))
      (chirp-stop)
      (when (buffer-live-p scratch)
        (kill-buffer scratch))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-thread-rerender-preserves-text-scale ()
  "A cached thread redraw should keep text scale."
  (save-window-excursion
    (let ((scratch (generate-new-buffer " *chirp-thread-scale*")) buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'chirp-media-avatar-image)
                     (lambda (&rest _args) nil)))
            (setq buffer
                  (chirp-thread-test--render-view
                   scratch "Thread" #'ignore
                   '((:kind tweet :id "1" :text "Hello"
                      :author-name "Alice" :author-handle "alice"))))
            (switch-to-buffer buffer)
            (text-scale-increase 2)
            (let ((amount text-scale-mode-amount)
                  (surface (appkit-current-surface)))
              (should (> amount 0))
              (appkit-loop-run-pass (appkit-surface-loop surface))
              (should (equal amount text-scale-mode-amount))
              (should (bound-and-true-p text-scale-mode))
              (should (string-match-p "Hello" (buffer-string)))))
        (chirp-stop)
        (when (buffer-live-p scratch) (kill-buffer scratch))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest chirp-thread-projection-draws-ancestor-chain-prefix ()
  "Ancestors should share a prefix spine and keep replies nested under the focus."
  (let ((appkit-discussion-connector-style 'text)
        (scratch (generate-new-buffer " *chirp-thread-chain-test*"))
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-media-avatar-image)
                   (lambda (&rest _args) nil)))
          (setq buffer
                (chirp-thread-test--render-view scratch
                                                "Thread"
                                                #'ignore
                                                '((:kind tweet :id "root" :text "Root"
                                                   :author-name "Alice" :author-handle "alice")
                                                  (:kind tweet :id "focus" :text "Focus"
                                                   :reply-to-id "root"
                                                   :author-name "Bob" :author-handle "bob")
                                                  (:kind tweet :id "reply" :text "Reply"
                                                   :reply-to-id "focus"
                                                   :author-name "Carol" :author-handle "carol"))
                                                nil nil "focus"))
          (with-current-buffer buffer
            (should (equal (plist-get (chirp-entry-at-point) :id) "focus"))
            (should (string-prefix-p
                     "│ " (get-text-property (point) 'line-prefix)))
            (goto-char (point-min))
            (should (equal (plist-get (chirp-entry-at-point) :id) "root"))
            (should (string-prefix-p
                     "│ " (get-text-property (point) 'line-prefix)))
            (goto-char (appkit-discussion-next-position))
            (goto-char (appkit-discussion-next-position))
            (should (equal (plist-get (chirp-entry-at-point) :id) "reply"))
            (should (equal "    "
                           (get-text-property (point) 'line-prefix)))))
      (chirp-stop)
      (when (buffer-live-p scratch)
        (kill-buffer scratch))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-thread-open-tweet-applies-prefetched-article-before-first-render ()
  "Article enrichment should overlap thread loading and feed the first render."
  (let ((buffer (generate-new-buffer " *chirp-thread-test*"))
        article-callback
        thread-callback
        rendered)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'thread-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'thread-token)))
                  ((symbol-function 'chirp-backend-article)
                   (lambda (_tweet-id callback &optional _errback)
                     (setq article-callback callback)))
                  ((symbol-function 'chirp-backend-thread)
                   (lambda (_target callback &optional _errback)
                     (setq thread-callback callback)))
                  ((symbol-function 'chirp-thread--present)
                   (lambda (_view ordered &optional _position)
                     (setq rendered ordered)))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
                  ((symbol-function 'chirp-display-buffer) #'ignore))
          (chirp-thread-open-tweet
           '(:kind tweet
             :id "123"
             :article-title "Article"
             :text ""
             :urls ("https://example.com/article")))
          (should (functionp article-callback))
          (should (functionp thread-callback))
          (funcall article-callback
                   '(:kind tweet
                     :id "123"
                     :article-title "Article"
                     :article-text "Full body")
                   nil)
          (funcall thread-callback
                   (list '(:kind tweet
                           :id "123"
                           :article-title "Article"
                           :text ""
                           :urls ("https://example.com/article"))
                         '(:kind tweet
                           :id "456"
                           :text "Reply"))
                   nil)
          (should (equal (plist-get (car rendered) :article-text) "Full body")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-thread-open-tweet-filters-keyword-spam-replies ()
  "Thread loading should hide matching replies without hiding the focus tweet."
  (let ((buffer (generate-new-buffer " *chirp-thread-spam-test*"))
        (chirp-spam-rules '("dm me" "t.me/" "推广昵称" "  "))
        (chirp-spam-rules-file nil)
        thread-callback
        rendered)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-begin-background-request)
                   (lambda (_buffer _title)
                     'thread-token))
                  ((symbol-function 'chirp-request-current-p)
                   (lambda (_buffer token)
                     (eq token 'thread-token)))
                  ((symbol-function 'chirp-backend-thread)
                   (lambda (_target callback &optional _errback)
                     (setq thread-callback callback)))
                  ((symbol-function 'chirp-backend-article) #'ignore)
                  ((symbol-function 'chirp-thread--present)
                   (lambda (_view ordered &optional _position)
                     (setq rendered ordered)))
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
                  ((symbol-function 'chirp-display-buffer) #'ignore))
          (chirp-thread-open-tweet
           '(:kind tweet :id "123" :text "DM me is quoted in the focus"))
          (should (functionp thread-callback))
          (funcall thread-callback
                   (list
                    '(:kind tweet :id "123" :text "DM me is quoted in the focus")
                    '(:kind tweet :id "spam-text" :text "Please DM ME for support")
                    '(:kind tweet :id "spam-url" :text "More details"
                      :urls ("https://t.me/example"))
                    '(:kind tweet :id "spam-author" :text "Ordinary reply"
                      :author-name "这是推广昵称")
                    '(:kind tweet :id "related" :text "DM me for context"
                      :timeline-context related)
                    '(:kind tweet :id "legit" :text "Useful reply"))
                   nil)
          (should (equal (mapcar (lambda (tweet) (plist-get tweet :id)) rendered)
                         '("123" "related" "legit"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-thread-user-spam-rules-share-reply-match-scope ()
  "One persistent rule set should inspect reply content and author identity."
  (let ((file (make-temp-file "chirp-spam-rules-"))
        (chirp-spam-rules '("built-in phrase")))
    (unwind-protect
        (let ((chirp-spam-rules-file file))
          (with-temp-file file
            (insert "local phrase\nPromo Name\nspam_handle\n"))
          (should
           (equal
            (mapcar
             (lambda (tweet) (plist-get tweet :id))
             (chirp-thread--filter-spam-replies
              (list '(:id "focus" :text "built-in phrase")
                    '(:id "built-in" :text "contains BUILT-IN PHRASE")
                    '(:id "text" :text "contains LOCAL PHRASE")
                    '(:id "name" :text "ordinary" :author-name "Promo Name 01")
                    '(:id "handle" :text "ordinary" :author-handle "Spam_Handle_01")
                    '(:id "related" :text "local phrase"
                      :timeline-context related)
                    '(:id "legit" :text "ordinary" :author-name "Alice"))))
            '("focus" "related" "legit"))))
      (delete-file file))))

(ert-deftest chirp-thread-user-spam-rules-respect-filter-disable ()
  "Setting the keyword option to nil should also disable persistent rules."
  (let ((file (make-temp-file "chirp-spam-rules-"))
        (chirp-spam-rules nil))
    (unwind-protect
        (let ((chirp-spam-rules-file file)
              (tweets (list '(:id "focus" :text "ordinary")
                            '(:id "reply" :text "local phrase"))))
          (with-temp-file file
            (insert "local phrase\n"))
          (should (eq (chirp-thread--filter-spam-replies tweets) tweets)))
      (delete-file file))))

(ert-deftest chirp-thread-add-spam-rule-persists-and-avoids-duplicates ()
  "Adding a rule should persist once and refresh only for a new rule."
  (let ((file (make-temp-file "chirp-spam-rules-"))
        (chirp-spam-rules '("built-in"))
        (refresh-count 0)
        initial-input)
    (unwind-protect
        (let ((chirp-spam-rules-file file))
          (with-temp-buffer
            (chirp-view-mode)
            (setq-local chirp-thread--refilter-function
                        (lambda () (cl-incf refresh-count)))
            (let ((inhibit-read-only t))
              (insert "Selected phrase"))
            (set-mark (point-min))
            (activate-mark)
            (let ((transient-mark-mode t))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (_prompt &optional initial-input-arg &rest _args)
                           (setq initial-input initial-input-arg)
                           "Selected phrase"))
                        ((symbol-function 'chirp-refresh)
                         (lambda () (ert-fail "Rule capture must not fetch"))))
                (chirp-thread-add-spam-rule)
                (should (equal initial-input "Selected phrase"))
                (should (= refresh-count 1))
                (should (equal (chirp-spam--read-user-rules)
                               '("Selected phrase")))
                (chirp-thread-add-spam-rule)
                (should (= refresh-count 1))
                (should (equal (chirp-spam--read-user-rules)
                               '("Selected phrase")))))))
      (delete-file file))))

(ert-deftest chirp-thread-spam-rule-suggestion-can-use-author ()
  "A prefix request should suggest the current author display name."
  (with-temp-buffer
    (cl-letf (((symbol-function 'chirp-entry-at-point)
               (lambda ()
                 '(:kind tweet :text "Reply text" :author-name "Promo Author"))))
      (should (equal (chirp-thread--spam-rule-suggestion nil) "Reply text"))
      (should (equal (chirp-thread--spam-rule-suggestion t) "Promo Author")))))

(ert-deftest chirp-thread-add-spam-rule-rejects-comments ()
  "Interactive additions should reject values parsed as file comments."
  (let ((chirp-spam-rules-file (make-temp-file "chirp-spam-rules-")))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _args) "# ignored")))
          (should-error (chirp-thread-add-spam-rule) :type 'user-error)
          (should (string-empty-p
                   (with-temp-buffer
                     (insert-file-contents chirp-spam-rules-file)
                     (buffer-string)))))
      (delete-file chirp-spam-rules-file))))

(ert-deftest chirp-view-mode-binds-spam-rule-capture ()
  "The documented spam capture key should invoke its command."
  (should (eq (lookup-key chirp-view-mode-map (kbd "S"))
              #'chirp-thread-add-spam-rule)))

(ert-deftest chirp-thread-edit-spam-rules-opens-configured-file ()
  "The edit command should create the parent directory and open the rule file."
  (let* ((directory (make-temp-file "chirp-spam-directory-" t))
         (nested-directory (expand-file-name "nested" directory))
         (file (expand-file-name "rules.txt" nested-directory))
         opened)
    (unwind-protect
        (let ((chirp-spam-rules-file file))
          (cl-letf (((symbol-function 'find-file)
                     (lambda (path)
                       (setq opened path))))
            (chirp-thread-edit-spam-rules)
            (should (file-directory-p nested-directory))
            (should (equal opened file))))
      (delete-directory directory t))))

(ert-deftest chirp-thread-refilters-retained-data-without-fetching ()
  "Rule capture updates rendered entries; disabling rules restores replies."
  (let ((chirp--app nil)
        (chirp-spam-rules '("existing spam"))
        (chirp-spam-rules-file (make-temp-file "chirp-spam-"))
        (requests 0) callback buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-backend-thread)
                   (lambda (_id success &optional _error)
                     (cl-incf requests) (setq callback success)))
                  ((symbol-function 'chirp-backend-article) #'ignore)
                  ((symbol-function 'chirp-media-prefetch-tweets) #'ignore)
                  ((symbol-function 'chirp-enrich-quoted-tweets) #'ignore)
                  ((symbol-function 'chirp-media-avatar-image) (lambda (&rest _) nil))
                  ((symbol-function 'chirp-display-buffer) #'ignore)
                  ((symbol-function 'read-string) (lambda (&rest _) "new spam")))
          (setq buffer (chirp-thread-open "focus"))
          (funcall callback
                   '((:kind tweet :id "root" :text "new spam ancestor")
                     (:kind tweet :id "focus" :reply-to-id "root" :text "new spam focus")
                     (:kind tweet :id "old" :reply-to-id "focus" :text "existing spam reply")
                     (:kind tweet :id "new" :reply-to-id "focus" :text "new spam reply")
                     (:kind tweet :id "related" :text "new spam related" :timeline-context related)
                     (:kind tweet :id "clean" :reply-to-id "focus" :text "clean reply")) nil)
          (with-current-buffer buffer
            (let* ((surface (appkit-current-surface))
                   (loop (appkit-surface-loop surface)))
              (cl-labels ((settle ()
                            (while (> (appkit-loop-pending-count loop) 0)
                              (appkit-loop-run-pass loop)))
                          (ids ()
                            (mapcar (lambda (tweet) (plist-get tweet :id))
                                    (plist-get (appkit-surface-model surface) :items))))
                (settle)
                (should (equal (ids) '("root" "focus" "new" "related" "clean")))
                (should (string-match-p "new spam reply" (buffer-string)))
                (chirp-thread-add-spam-rule)
                (settle)
                (should (equal (ids) '("root" "focus" "related" "clean")))
                (should-not (string-match-p "new spam reply" (buffer-string)))
                (should (string-match-p "clean reply" (buffer-string)))
                (let ((chirp-spam-rules nil))
                  (funcall chirp-thread--refilter-function))
                (settle)
                (should (equal (ids) '("root" "focus" "old" "new" "related" "clean")))
                (should (string-match-p "existing spam reply" (buffer-string)))
                (should (string-match-p "new spam reply" (buffer-string)))
                (should (= requests 1))))))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-file chirp-spam-rules-file))))

(provide 'chirp-thread-test)

;;; chirp-thread-test.el ends here
