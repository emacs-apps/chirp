;;; chirp-unsent-test.el --- Tests for unsent draft lists -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-backend)
(require 'chirp-unsent)
(require 'chirp-actions)

(unless (fboundp 'chirp-test--make-compose-buffer)
  (load (expand-file-name "chirp-actions-test.el"
                          (file-name-directory (or load-file-name
                                                   default-directory)))
        nil t))
(declare-function chirp-test--make-compose-buffer
                  "chirp-actions-test" (body))

(defun chirp-unsent-test--payload-at-path (path value)
  "Return a JSON-style payload containing VALUE at PATH."
  (dolist (key (reverse (copy-sequence path)) value)
    (setq value (list (cons key value)))))

(defun chirp-unsent-test--draft-payload
    (status &optional thread-status media-id preview-url)
  "Return a FetchDraftTweets payload with STATUS and optional THREAD-STATUS.

When MEDIA-ID is set, include that media ID and optional PREVIEW-URL."
  (let* ((thread (if thread-status
                     (vector `(("status" . ,thread-status)
                               ("media_ids" . [])))
                   []))
         (media-ids (if media-id (vector media-id) []))
         (entities
          (if media-id
              (vector
               `(("media_key" . ,(format "3_%s" media-id))
                 ("media_info" .
                  (("__typename" . "ApiImage")
                   ("original_img_url"
                    . ,(or preview-url
                           "https://pbs.twimg.com/media/x.jpg"))
                   ("original_img_width" . 1200)
                   ("original_img_height" . 800)))))
            [])))
    (chirp-unsent-test--payload-at-path
     '("data" "viewer" "draft_list" "response_data")
     (vector
      `(("rest_id" . "2087")
        ("media_entities" . ,entities)
        ("tweet_create_request" .
         (("status" . ,status)
          ("media_ids" . ,media-ids)
          ("thread_tweets" . ,thread))))))))

(defun chirp-unsent-test--scheduled-payload (status execute-at)
  "Return a FetchScheduledTweets payload with STATUS and EXECUTE-AT."
  (chirp-unsent-test--payload-at-path
   '("data" "viewer" "scheduled_tweet_list")
   (vector
    `(("rest_id" . "2090")
      ("scheduling_info" .
       (("execute_at" . ,execute-at)
        ("state" . "Scheduled")))
      ("tweet_create_request" .
       (("status" . ,status)
        ("media_ids" . [])))))))

(ert-deftest chirp-backend-fetch-unsent-normalizes-a-thread-draft ()
  "FetchDraftTweets should expose root text, thread texts, and IDs."
  (let (entries)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (operation variables callback &rest _options)
                 (should (equal (plist-get operation :name) "FetchDraftTweets"))
                 (should (eq (chirp-get variables "ascending") :json-false))
                 (funcall callback
                          (chirp-unsent-test--draft-payload
                           "Nice" "Two Nices")))))
      (chirp-backend-fetch-unsent
       'draft
       (lambda (result _envelope)
         (setq entries result))))
    (should (= (length entries) 1))
    (should (equal (plist-get (car entries) :id) "2087"))
    (should (eq (plist-get (car entries) :kind) 'draft))
    (should (eq (plist-get (car entries) :compose-kind) 'post))
    (should (equal (plist-get (car entries) :texts)
                   '("Nice" "Two Nices")))))

(ert-deftest chirp-backend-fetch-unsent-keeps-media-ids-and-previews ()
  "Draft media_entities should become reusable compose attachments."
  (let (entries)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (_operation _variables callback &rest _options)
                 (funcall callback
                          (chirp-unsent-test--draft-payload
                           "Nice" nil "2087634413287100416"
                           "https://pbs.twimg.com/media/HPjEcCjb0AA6uAy.jpg")))))
      (chirp-backend-fetch-unsent
       'draft
       (lambda (result _envelope)
         (setq entries result))))
    (let* ((item (car (plist-get (car entries) :items)))
           (attachment (car (plist-get item :attachments))))
      (should (equal (plist-get (car entries) :media-count) 1))
      (should (equal (plist-get attachment :media-id) "2087634413287100416"))
      (should (equal (plist-get attachment :preview-url)
                     "https://pbs.twimg.com/media/HPjEcCjb0AA6uAy.jpg")))))

(ert-deftest chirp-backend-fetch-unsent-normalizes-a-scheduled-post ()
  "FetchScheduledTweets should keep execute_at and status."
  (let (entries)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (operation variables callback &rest _options)
                 (should (equal (plist-get operation :name)
                                "FetchScheduledTweets"))
                 (should (eq (chirp-get variables "ascending") t))
                 (funcall callback
                          (chirp-unsent-test--scheduled-payload
                           "later" 1786700000000)))))
      (chirp-backend-fetch-unsent
       'scheduled
       (lambda (result _envelope)
         (setq entries result))))
    (should (equal (plist-get (car entries) :id) "2090"))
    (should (eq (plist-get (car entries) :kind) 'scheduled))
    (should (equal (plist-get (car entries) :execute-at) 1786700000))
    (should (equal (plist-get (car entries) :texts) '("later")))))

(ert-deftest chirp-backend-unix-seconds-accepts-milliseconds ()
  "Scheduled execute_at values from X are milliseconds."
  (should (equal (chirp-backend--unix-seconds 1786572521000) 1786572521))
  (should (equal (chirp-backend--unix-seconds 1786572521) 1786572521)))

(ert-deftest chirp-backend-delete-unsent-uses-kind-specific-ids ()
  "Deletes should send draft_tweet_id or scheduled_tweet_id."
  (let (operation variables)
    (cl-letf (((symbol-function 'chirp-x-graphql-request)
               (lambda (request-operation request-variables callback
                                          &rest _options)
                 (setq operation request-operation
                       variables request-variables)
                 (funcall callback '(("data" . nil))))))
      (chirp-backend-delete-unsent 'draft "2087" #'ignore)
      (should (equal (plist-get operation :name) "DeleteDraftTweet"))
      (should (equal (chirp-get variables "draft_tweet_id") "2087"))
      (chirp-backend-delete-unsent 'scheduled "2090" #'ignore)
      (should (equal (plist-get operation :name) "DeleteScheduledTweet"))
      (should (equal (chirp-get variables "scheduled_tweet_id") "2090")))))

(ert-deftest chirp-unsent-rows-use-buffer-menu-mark-column ()
  "The unsent list should keep a Buffer Menu style mark column."
  (let ((buffer (generate-new-buffer " *chirp-unsent-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-unsent-mode)
          (setq-local chirp-unsent-kind 'draft)
          (chirp-unsent--apply-entries
           (list (list :id "2087"
                       :kind 'draft
                       :texts '("Nice" "Two Nices")
                       :media-count 0)))
          (goto-char (point-min))
          (should (equal (tabulated-list-get-id) "2087"))
          (chirp-unsent-flag-delete)
          (goto-char (point-min))
          (should (equal (chirp-unsent--flagged-ids ?D) '("2087")))
          (chirp-unsent-unmark-all)
          (should-not (chirp-unsent--flagged-ids ?D)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-unsent-rows-keep-a-trailing-when-column ()
  "Unsent rows should retain preview text before the trailing When column."
  (let ((buffer (generate-new-buffer " *chirp-unsent-columns-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (chirp-unsent-mode)
          (setq-local chirp-unsent-kind 'scheduled)
          (chirp-unsent--apply-entries
           (list (list :id "2090"
                       :kind 'scheduled
                       :texts '("A long scheduled post preview")
                       :execute-at 1786572521)))
          (goto-char (point-min))
          (search-forward "A long scheduled post preview")
          (search-forward "2026-")
          (should (equal
                   (get-text-property
                    (match-beginning 0) 'tabulated-list-column-name)
                   "When")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chirp-compose-open-unsent-restores-thread-text ()
  "Opening an unsent draft should restore every part and the draft ID."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "ignore")))
    (unwind-protect
        (with-current-buffer compose
          (chirp-compose--apply-unsent
           (list :id "2087"
                 :kind 'draft
                 :compose-kind 'post
                 :texts '("Nice" "Two Nices")
                 :media-count 0))
          (should (equal chirp-compose-draft-id "2087"))
          (should (= (length (appkit-chat-compose-items)) 2))
          (should (equal (appkit-chat-compose-bodies) '("Nice" "Two Nices"))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest chirp-compose-open-unsent-restores-media-ids ()
  "Opening an unsent draft should keep X media IDs for the next submit."
  (pcase-let ((`(,compose . ,source)
               (chirp-test--make-compose-buffer "ignore")))
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-media-prefetch-file)
                   (lambda (&rest _args) nil)))
          (with-current-buffer compose
            (chirp-compose--apply-unsent
             (list :id "2087"
                   :kind 'draft
                   :compose-kind 'post
                   :items
                   (list (list :text "Nice"
                               :attachments
                               (list (list :media-id "2087634413287100416"
                                           :preview-url
                                           "https://pbs.twimg.com/media/x.jpg"))))))
            (should (equal (plist-get
                            (car (chirp-compose--item-attachments))
                            :media-id)
                           "2087634413287100416"))
            (let ((draft (chirp-compose--draft)))
              (should (equal (plist-get
                              (car (plist-get (car (plist-get draft :items))
                                              :attachments))
                              :media-id)
                             "2087634413287100416")))))
      (when (buffer-live-p compose)
        (kill-buffer compose))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(defun chirp-unsent-test--entry (id &optional text kind)
  "Return a normalized unsent entry for ID, TEXT and KIND."
  (list :id id :kind (or kind 'draft) :compose-kind 'post
        :items (list (list :text (or text id) :attachments nil))
        :texts (list (or text id)) :media-count 0))

(defmacro chirp-unsent-test--with-list (entries &rest body)
  "Run BODY in a temporary unsent list initialized with ENTRIES."
  (declare (indent 1))
  `(with-temp-buffer
     (chirp-unsent-mode)
     (unwind-protect
         (progn
           (chirp-unsent--apply-entries ,entries)
           (goto-char (point-min))
           ,@body)
       (chirp-clear-status))))

(defun chirp-unsent-test--goto (id)
  "Move to the displayed row for ID, failing if it is missing."
  (goto-char (point-min))
  (while (and (not (eobp)) (not (equal id (tabulated-list-get-id))))
    (forward-line 1))
  (should (equal id (tabulated-list-get-id))))

(ert-deftest chirp-unsent-delete-commits-progress-and-retains-other-marks ()
  "An acknowledged deletion survives partial failure without a reload."
  (chirp-unsent-test--with-list
      (mapcar #'chirp-unsent-test--entry '("a" "b" "c" "other"))
    (let (requests error-message)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'chirp-backend-fetch-unsent)
                 (lambda (&rest _) (ert-fail "Unexpected list reload")))
                ((symbol-function 'chirp-backend-delete-unsent)
                 (lambda (kind id callback errback)
                   (setq requests
                         (append requests (list (list kind id callback errback))))))
                ((symbol-function 'chirp-actions-show-error)
                 (lambda (message) (setq error-message message))))
        (dotimes (_ 3) (chirp-unsent-flag-delete))
        (chirp-unsent-execute)
        ;; Local work done while the request is pending must also survive.
        (chirp-unsent-test--goto "other")
        (chirp-unsent-mark)
        (chirp-unsent-test--goto "other")
        (let ((column (current-column)))
          (funcall (nth 2 (car requests)) nil nil)
          (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                                chirp-unsent-entries)
                         '("b" "c" "other")))
          (should (equal (tabulated-list-get-id) "other"))
          (should (= column (current-column))))
        (should (equal (chirp-unsent--flagged-ids ?>) '("other")))
        (should (equal (chirp-unsent--flagged-ids ?D) '("b" "c")))
        (funcall (nth 3 (cadr requests)) "Delete failed")
        (should (equal error-message "Delete failed"))
        (should (equal (mapcar (lambda (request)
                                (list (car request) (cadr request)))
                              requests)
                       '((draft "a") (draft "b"))))
        (should (equal (chirp-unsent--flagged-ids ?D) '("b" "c")))
        (should (equal (chirp-unsent--flagged-ids ?>) '("other")))
        (should-not chirp--status-text)))))

(ert-deftest chirp-unsent-kind-switch-removes-actionable-old-rows ()
  "Pending and failed scheduled fetches cannot dispatch draft row IDs."
  (chirp-unsent-test--with-list (list (chirp-unsent-test--entry "draft-id"))
    (let ((buffer (current-buffer)) failure)
      (cl-letf (((symbol-function 'chirp-unsent--buffer) (lambda () buffer))
                ((symbol-function 'pop-to-buffer) #'ignore)
                ((symbol-function 'chirp-backend-fetch-unsent)
                 (lambda (kind _callback errback)
                   (should (eq kind 'scheduled))
                   (setq failure errback)))
                ((symbol-function 'chirp-backend-delete-unsent)
                 (lambda (&rest _) (ert-fail "Dispatched an old-kind ID")))
                ((symbol-function 'chirp-actions-show-error) #'ignore))
        (chirp-unsent-flag-delete)
        (chirp-unsent-toggle-kind)
        (should (eq chirp-unsent-kind 'scheduled))
        (should-not chirp-unsent-entries)
        (should-not (tabulated-list-get-id))
        (should-error (chirp-unsent-execute) :type 'user-error)
        (funcall failure "Fetch failed")
        (should-not chirp-unsent-entries)
        (should-error (chirp-unsent-open) :type 'user-error)
        (should-error (chirp-unsent-execute) :type 'user-error)))))

(ert-deftest chirp-unsent-replaced-controller-rejects-fetch-and-delete-results ()
  "Old callbacks cannot modify or clear status in a replacement controller."
  (chirp-unsent-test--with-list
      (mapcar #'chirp-unsent-test--entry '("a" "b"))
    (let (fetched fetch-failed requests)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'chirp-backend-fetch-unsent)
                 (lambda (_kind callback errback)
                   (setq fetched callback fetch-failed errback)))
                ((symbol-function 'chirp-backend-delete-unsent)
                 (lambda (kind id callback errback)
                   (setq requests
                         (append requests (list (list kind id callback errback))))))
                ((symbol-function 'chirp-actions-show-error)
                 (lambda (&rest _) (ert-fail "Stale controller error"))))
        (chirp-unsent-refresh)
        (dotimes (_ 2) (chirp-unsent-flag-delete))
        (chirp-unsent-execute)
        (chirp-clear-status)
        (chirp-unsent-mode)
        (should-not chirp-unsent-entries)
        (should-not (tabulated-list-get-id))
        (setq chirp-unsent-kind 'scheduled)
        (chirp-unsent--apply-entries
         (list (chirp-unsent-test--entry "a" "Replacement" 'scheduled)))
        (chirp-set-status (current-buffer) "Replacement loading")
        (funcall fetched (list (chirp-unsent-test--entry "obsolete")) nil)
        (funcall fetch-failed "Old fetch failed")
        (funcall (nth 2 (car requests)) nil nil)
        ;; The approved batch retains its original endpoint, even after reuse.
        (should (equal (mapcar (lambda (request)
                                (list (car request) (cadr request)))
                              requests)
                       '((draft "a") (draft "b"))))
        (funcall (nth 3 (cadr requests)) "Old delete failed")
        (should (equal (mapcar (lambda (entry) (plist-get entry :texts))
                              chirp-unsent-entries)
                       '(("Replacement"))))
        (should (equal chirp--status-text "Replacement loading"))))))

(ert-deftest chirp-unsent-major-mode-change-revokes-callbacks ()
  "A former unsent buffer cannot receive delayed list or deletion effects."
  (chirp-unsent-test--with-list (list (chirp-unsent-test--entry "a"))
    (let (fetched deleted)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'chirp-backend-fetch-unsent)
                 (lambda (_kind callback _errback) (setq fetched callback)))
                ((symbol-function 'chirp-backend-delete-unsent)
                 (lambda (_kind _id callback _errback) (setq deleted callback))))
        (chirp-unsent-refresh)
        (chirp-unsent-flag-delete)
        (chirp-unsent-execute)
        (chirp-clear-status)
        (fundamental-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Unrelated buffer"))
        (funcall fetched (list (chirp-unsent-test--entry "old")) nil)
        (funcall deleted nil nil)
        (should (eq major-mode 'fundamental-mode))
        (should (equal (buffer-string) "Unrelated buffer"))))))

(ert-deftest chirp-unsent-compose-results-update-only-matching-source ()
  "Confirmed compose snapshots update and delete rows without losing marks."
  (chirp-unsent-test--with-list
      (mapcar #'chirp-unsent-test--entry '("a" "other"))
    (let ((source (chirp-unsent-capture-source (current-buffer))))
      (cl-letf (((symbol-function 'chirp-backend-fetch-unsent)
                 (lambda (&rest _) (ert-fail "Unexpected compose reload"))))
        ;; Sorted lists must also repaint edited rows, not just retain IDs.
        (setq tabulated-list-sort-key '("Text"))
        (tabulated-list-print t)
        (chirp-unsent-test--goto "other")
        (chirp-unsent-flag-delete)
        (chirp-unsent-test--goto "other")
        (chirp-unsent-apply-result source 'draft "a"
                                  (chirp-unsent-test--entry "a" "Edited"))
        (should (equal (tabulated-list-get-id) "other"))
        (should (equal (chirp-unsent--flagged-ids ?D) '("other")))
        (chirp-unsent-test--goto "a")
        (should (equal (plist-get (chirp-unsent--entry-at-point) :texts)
                       '("Edited")))
        (should (equal (aref (tabulated-list-get-entry) 2) "Edited"))
        (chirp-unsent-apply-result source 'draft "new"
                                  (chirp-unsent-test--entry "new" "Saved"))
        (should (equal (tabulated-list-get-id) "a"))
        (chirp-unsent-test--goto "new")
        (should (equal (plist-get (chirp-unsent--entry-at-point) :texts)
                       '("Saved")))
        (chirp-unsent-apply-result source 'scheduled "a")
        (chirp-unsent-test--goto "a")
        (should (equal (plist-get (chirp-unsent--entry-at-point) :texts)
                       '("Edited")))
        (chirp-unsent-test--goto "other")
        (chirp-unsent-apply-result source 'draft "a")
        (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                              chirp-unsent-entries)
                       '("new" "other")))
        (should (equal (tabulated-list-get-id) "other"))
        (should (equal (chirp-unsent--flagged-ids ?D) '("other")))
        (chirp-unsent-mode)
        (chirp-unsent--apply-entries (list (chirp-unsent-test--entry "new")))
        (chirp-unsent-apply-result source 'draft "new")
        (should (equal (plist-get (car chirp-unsent-entries) :id) "new"))))))

(ert-deftest chirp-unsent-kind-round-trip-revokes-old-compose-source ()
  "Leaving and returning to Drafts does not revive an old source reference."
  (chirp-unsent-test--with-list (list (chirp-unsent-test--entry "a"))
    (let ((source (chirp-unsent-capture-source (current-buffer)))
          (buffer (current-buffer)))
      (cl-letf (((symbol-function 'chirp-unsent--buffer) (lambda () buffer))
                ((symbol-function 'pop-to-buffer) #'ignore)
                ((symbol-function 'chirp-backend-fetch-unsent)
                 (lambda (kind callback _errback)
                   (funcall callback
                            (list (chirp-unsent-test--entry "a" "Current" kind))
                            nil))))
        (chirp-unsent-open-kind 'scheduled)
        (chirp-unsent-open-kind 'draft)
        (chirp-unsent-apply-result source 'draft "a")
        (should (equal (plist-get (car chirp-unsent-entries) :texts)
                       '("Current")))))))

(ert-deftest chirp-unsent-fetch-retains-intervening-confirmed-writes ()
  "An older server snapshot cannot undo a locally confirmed upsert or delete."
  (chirp-unsent-test--with-list
      (mapcar #'chirp-unsent-test--entry '("a" "b" "other"))
    (let ((source (chirp-unsent-capture-source (current-buffer))) fetched)
      (cl-letf (((symbol-function 'chirp-backend-fetch-unsent)
                 (lambda (_kind callback _errback) (setq fetched callback))))
        (chirp-unsent-refresh)
        (chirp-unsent-test--goto "other")
        (chirp-unsent-mark)
        (chirp-unsent-apply-result source 'draft "a")
        (chirp-unsent-apply-result source 'draft "b"
                                  (chirp-unsent-test--entry "b" "Saved"))
        (funcall fetched
                 (mapcar #'chirp-unsent-test--entry '("a" "b" "other" "server"))
                 nil)
        (should (equal (mapcar (lambda (entry) (plist-get entry :id))
                              chirp-unsent-entries)
                       '("b" "other" "server")))
        (should (equal (plist-get (car chirp-unsent-entries) :texts) '("Saved")))
        (should (equal (chirp-unsent--flagged-ids ?>) '("other")))))))

(ert-deftest chirp-unsent-delete-completion-does-not-clear-newer-fetch-status ()
  "An earlier delete cannot clear loading status owned by an explicit refresh."
  (chirp-unsent-test--with-list (list (chirp-unsent-test--entry "a"))
    (let (deleted fetched)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'chirp-backend-delete-unsent)
                 (lambda (_kind _id callback _errback) (setq deleted callback)))
                ((symbol-function 'chirp-backend-fetch-unsent)
                 (lambda (_kind callback _errback) (setq fetched callback))))
        (chirp-unsent-flag-delete)
        (chirp-unsent-execute)
        (chirp-unsent-refresh)
        (funcall deleted nil nil)
        (should-not chirp-unsent-entries)
        (should (equal chirp--status-text "Loading Drafts..."))
        (funcall fetched (list (chirp-unsent-test--entry "a")) nil)
        (should-not chirp-unsent-entries)
        (should-not chirp--status-text)))))

(ert-deftest chirp-unsent-navigation-fences-pending-delete-completion ()
  "A departed collection's delete cannot replace or clear the new fetch."
  (chirp-unsent-test--with-list (list (chirp-unsent-test--entry "same-id"))
    (let ((buffer (current-buffer)) deleted fetched)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'chirp-unsent--buffer) (lambda () buffer))
                ((symbol-function 'pop-to-buffer) #'ignore)
                ((symbol-function 'chirp-backend-delete-unsent)
                 (lambda (kind id callback _errback)
                   (should (eq kind 'draft))
                   (should (equal id "same-id"))
                   (setq deleted callback)))
                ((symbol-function 'chirp-backend-fetch-unsent)
                 (lambda (kind callback _errback)
                   (should (eq kind 'scheduled))
                   (should-not fetched)
                   (setq fetched callback))))
        (chirp-unsent-flag-delete)
        (chirp-unsent-execute)
        (chirp-unsent-toggle-kind)
        (funcall deleted nil nil)
        (should (equal chirp--status-text "Loading Scheduled..."))
        (funcall fetched
                 (list (chirp-unsent-test--entry "same-id" "Scheduled" 'scheduled))
                 nil)
        (should (equal (plist-get (car chirp-unsent-entries) :texts)
                       '("Scheduled")))
        (should-not chirp--status-text)))))

(provide 'chirp-unsent-test)

;;; chirp-unsent-test.el ends here
