;;; chirp-dm-live-test.el --- Tests for XChat realtime delivery -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Exercise exact websocket ownership and canonical live-event publication.

;;; Code:

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'chirp)
(require 'chirp-dm-live)
(require 'chirp-dm-test-helper)

(ert-deftest chirp-dm-live-service-owns-one-exact-websocket ()
  "The Chirp app should own one fenced websocket and close it on shutdown."
  (let ((chirp--app nil) opened-options closed reconnect)
    (cl-letf
        (((symbol-function 'chirp-backend-dm-live-token)
          (lambda (callback &rest _options)
            (funcall callback "header.payload.signature" nil)
            'token-request))
         ((symbol-function 'chirp-x-chat-live-open)
          (lambda (_token on-open on-message on-close on-error)
            (setq opened-options
                  (list on-open on-message on-close on-error))
            'live-socket))
         ((symbol-function 'chirp-x-chat-live-close)
          (lambda (socket) (push socket closed)))
         ((symbol-function 'run-with-timer)
          (lambda (&rest _arguments) 'keepalive-timer))
         ((symbol-function 'run-at-time)
          (lambda (&rest arguments) (setq reconnect arguments)
            'reconnect-timer))
         ((symbol-function 'timerp)
          (lambda (value)
            (memq value '(keepalive-timer reconnect-timer)))))
      (unwind-protect
          (let ((service (chirp-dm-live-ensure)))
            (should
             (eq (chirp-dm-live--service-socket service) 'live-socket))
            (should
             (eq service
                 (chirp--session-dm-live
                  (appkit-app-model (chirp-app)))))
            (funcall (nth 2 opened-options) 'stale-socket)
            (should-not reconnect))
        (chirp-stop)))
    (should (memq 'live-socket closed))))

(ert-deftest chirp-dm-live-event-updates-open-canonical-conversation
    ()
  "A websocket event should appear through the open canonical conversation."
  (let ((chirp--app nil) buffer)
    (unwind-protect
        (let*
            ((old (chirp-dm-test--normalized-event "20" "20" "old"))
             (conversation
              (chirp-dm-test--normalized-conversation old))
             (encoded
              (chirp-dm-test--event
               :sequence "31"
               :message-id "message-31"
               :sender-id "42"
               :conversation-id "conversation-1"
               :text "live message"))
             (event (chirp-xchat-decode-event encoded)))
          (setq buffer (chirp-dm-conversation-open conversation))
          (let
              ((service
                (chirp-dm-live--service-create
                 :app (chirp-app)
                 :pending-conversations
                 (make-hash-table :test
                                  #'equal))))
            (chirp-dm-live--accept-event service event))
          (with-current-buffer buffer
            (let ((view (appkit-current-surface)))
              (appkit-loop-run-pass (appkit-surface-loop view))
              (let*
                  ((state (appkit-surface-model view))
                   (events
                    (plist-get (plist-get state :conversation) :events)))
                (should
                 (equal
                  (mapcar (lambda (item) (plist-get item :id)) events)
                  '("20" "31")))
                (should
                 (string-match-p "live message" (buffer-string)))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-dm-live-decrypts-only-new-encrypted-events ()
  "A live decrypt should not submit already verified ciphertext again."
  (let ((chirp--app nil) buffer decrypted-inputs)
    (unwind-protect
        (let*
            ((old
              (chirp-dm-test--normalized-event "20" "20"
                                               "old verified"))
             (_old-state
              (setf (plist-get old :encoded-event) "encoded-old"
                    (plist-get old :decrypted-p) t))
             (conversation
              (chirp-dm-test--normalized-conversation old))
             (live
              (chirp-dm-test--normalized-event "31" "31"
                                               "[Encrypted message unavailable]"))
             (_live-state
              (setf (plist-get live :message-id) "message-31"
                    (plist-get live :encoded-event) "encoded-new"
                    (plist-get live :encrypted-p) t)))
          (setf (plist-get conversation :has-more) nil
                (plist-get conversation :older-cursor) nil)
          (cl-letf
              (((symbol-function 'chirp-backend-dm-signing-keys)
                (lambda (_user-ids callback &rest _options)
                  (funcall callback [] nil)
                  nil))
               ((symbol-function 'chirp-xchat-native-decrypt-events)
                (lambda (conversation-id events _signing-keys)
                  (should (equal conversation-id "conversation-1"))
                  (push events decrypted-inputs)
                  '((:sequence-id "31" :message-id "message-31"
                     :conversation-id "conversation-1" :content-kind
                     text :text "new verified" :attachments nil
                     :reply-p nil :reply-text nil
                     :reply-attachment-count 0)))))
            (setq buffer (chirp-dm-conversation-open conversation))
            (let
                ((service
                  (chirp-dm-live--service-create
                   :app (chirp-app)
                   :pending-conversations
                   (make-hash-table
                    :test #'equal))))
              (chirp-dm-live--accept-event service live))
            (let ((run-timer (symbol-function 'run-at-time))
                  wakeups)
              (cl-letf (((symbol-function 'run-at-time)
                         (lambda (time repeat function &rest arguments)
                           (if (and (equal time 0) (null repeat)
                                    (functionp function) (not (symbolp function)))
                               (progn
                                 (push (cons function arguments) wakeups)
                                 (timer-create))
                             (apply run-timer time repeat function arguments)))))
                (chirp-dm-test--drain
                 (with-current-buffer buffer (appkit-current-surface))))
              ;; Execute captured wakeups outside the runtime drain, without
              ;; depending on wall-clock scheduling in a batch test.
              (dolist (wakeup (nreverse wakeups))
                (apply (car wakeup) (cdr wakeup)))
              (chirp-dm-test--drain
               (with-current-buffer buffer (appkit-current-surface))))
            (should (equal decrypted-inputs '(("encoded-new"))))
            (should
             (equal
              (mapcar (lambda (event) (plist-get event :text))
                      (plist-get
                       (plist-get
                        (appkit-surface-model
                         (with-current-buffer buffer
                           (appkit-current-surface)))
                        :conversation)
                       :events))
              '("old verified" "new verified")))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-dm-live-event-promotes-its-canonical-inbox-row ()
  "A live event should move its existing canonical conversation to recent."
  (let ((chirp--app nil) buffer)
    (unwind-protect
        (let*
            ((first-event
              (chirp-dm-test--normalized-event "20" "20" "first" "42"
                                               "conversation-1"))
             (second-event
              (chirp-dm-test--normalized-event "21" "21" "second" "42"
                                               "conversation-2"))
             (first
              (chirp-dm-test--normalized-conversation first-event))
             (second
              (chirp-dm-test--normalized-conversation second-event))
             (live
              (chirp-dm-test--normalized-event "31" "31" "new second"
                                               "42" "conversation-2")))
          (setf (plist-get second :id) "conversation-2")
          (cl-letf
              (((symbol-function 'chirp-backend-dm-inbox)
                (lambda (callback &rest _options)
                  (funcall callback (list first second) nil)
                  'request)))
            (setq buffer (chirp-dm-inbox-open)))
          (with-current-buffer buffer
            (let*
                ((view (appkit-current-surface))
                 (state (appkit-surface-model view)))
              (should
               (equal
                (mapcar (lambda (item) (plist-get item :id))
                        (plist-get state :items))
                '("conversation-1" "conversation-2")))
              (chirp-dm-test--drain view)
              (should (chirp-dm-state-accept-live-event live))
              ;; Cross-Surface membership changes belong to inbox update.
              (should
               (equal (mapcar (lambda (item) (plist-get item :id))
                              (plist-get state :items))
                      '("conversation-1" "conversation-2")))
              (appkit-loop-run-pass (appkit-surface-loop view))
              (should
               (equal
                (mapcar (lambda (item) (plist-get item :id))
                        (plist-get state :items))
                '("conversation-2" "conversation-1")))
              (should (string-match-p "new second" (buffer-string))))))
      (chirp-stop) (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-dm-live-first-inbox-row-updates-without-unrelated-printing ()
  "An already-first row must repaint new activity, retaining unrelated rows."
  (let ((chirp--app nil) buffer printed)
    (unwind-protect
        (let* ((first (chirp-dm-test--normalized-conversation
                       (chirp-dm-test--normalized-event "20" "20" "old first")))
               (second (chirp-dm-test--normalized-conversation
                        (chirp-dm-test--normalized-event
                         "21" "21" "unchanged second" "42" "conversation-2")))
               (live (chirp-dm-test--normalized-event "31" "31" "new first")))
          (setf (plist-get second :id) "conversation-2")
          (cl-letf (((symbol-function 'chirp-backend-dm-inbox)
                     (lambda (callback &rest _options)
                       (funcall callback (list first second) nil) nil)))
            (setq buffer (chirp-dm-inbox-open)))
          (with-current-buffer buffer
            (let* ((view (appkit-current-surface))
                   (inserter (symbol-function 'chirp-dm-inbox--insert-item))
                   retained)
              (chirp-dm-test--drain view)
              (should (string-match-p "old first" (buffer-string)))
              (setq retained
                    (gethash '(dm-conversation "conversation-2")
                             (appkit-directory-surface-node-table
                              (appkit-directory-surface))))
              (cl-letf (((symbol-function 'chirp-dm-inbox--insert-item)
                         (lambda (surface entry)
                           (push (appkit-directory-entry-key entry) printed)
                           (funcall inserter surface entry))))
                (chirp-dm-state-accept-live-event live)
                (chirp-dm-test--drain view))
              (should (string-match-p "new first" (buffer-string)))
              (should-not (string-match-p "old first" (buffer-string)))
              (should (string-match-p "unchanged second" (buffer-string)))
              (should (equal printed '((dm-conversation "conversation-1"))))
              (should
               (eq retained
                   (gethash '(dm-conversation "conversation-2")
                            (appkit-directory-surface-node-table
                             (appkit-directory-surface))))))))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest chirp-dm-live-first-event-seeds-each-empty-window ()
  "First accepted activity should seed every empty window without losing drafts."
  (let ((chirp--app nil) buffers)
    (unwind-protect
        (let* ((conversation (chirp-dm-test--normalized-conversation))
               (_ (setf (plist-get conversation :has-more) nil))
               (first (chirp-dm-conversation-open conversation))
               (second (chirp-dm-conversation-open conversation))
               (event (chirp-dm-test--normalized-event "31" "31" "first live"))
               (service (chirp-dm-live--service-create
                         :app (chirp-app)
                         :pending-conversations (make-hash-table :test #'equal))))
          (setq buffers (list first second))
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (chirp-dm-test--drain (appkit-current-surface))
              (should-not (appkit-chat-timeline-node "31"))
              (goto-char (point-max))
              (insert "keep draft")))
          (chirp-dm-live--accept-event service event)
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (chirp-dm-test--drain (appkit-current-surface))
              (should (appkit-chat-timeline-node "31"))
              (should (string-match-p "first live" (buffer-string)))
              (should (equal (appkit-chatbuf-input-string) "keep draft")))))
      (chirp-stop)
      (dolist (buffer buffers)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest chirp-dm-delivery-commits-only-to-its-original-app ()
  "Inbox and live callbacks must not resolve canonical state through another App."
  (let ((chirp--app nil) buffer callback other-app)
    (unwind-protect
        (cl-letf (((symbol-function 'chirp-backend-dm-inbox)
                   (lambda (success &rest _options)
                     (setq callback success) nil)))
          (setq buffer (chirp-dm-inbox-open))
          (let* ((view (with-current-buffer buffer (appkit-current-surface)))
                 (original-app (appkit-surface-app view))
                 (service (chirp-dm-live--service-create
                           :app original-app
                           :pending-conversations (make-hash-table :test #'equal))))
            (setq other-app (let ((chirp--app nil)) (chirp-app)))
            (let ((chirp--app other-app))
              (funcall callback
                       (list (chirp-dm-test--normalized-conversation
                              (chirp-dm-test--normalized-event "20" "20" "old")))
                       nil)
              (chirp-dm-live--accept-event
               service (chirp-dm-test--normalized-event "31" "31" "owned live")))
            (chirp-dm-test--drain view)
            (should
             (equal
              (mapcar #'chirp-dm-state--event-id
                      (plist-get
                       (gethash "conversation-1"
                                (chirp--session-dm-conversations
                                 (appkit-app-model original-app)))
                       :events))
              '("20" "31")))
            (should-not
             (gethash "conversation-1"
                      (chirp--session-dm-conversations
                       (appkit-app-model other-app))))
            (with-current-buffer buffer
              (should (string-match-p "owned live" (buffer-string))))))
      (when (appkit-app-live-p other-app) (appkit-app-close other-app))
      (chirp-stop)
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(provide 'chirp-dm-live-test)

;;; chirp-dm-live-test.el ends here
