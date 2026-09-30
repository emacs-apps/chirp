;;; chirp-x-transaction-source.el --- X transaction metadata retrieval -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Retrieve public verification and animation material for GraphQL writes.
;; Only the X home request carries cookies; the static script request does not.
;; Keep one cookie-scoped material cache in memory.  Each preparation returns
;; a cancellation closure; the caller owns the overall write lifecycle.

;;; Code:

(require 'cl-lib)
(require 'plz)
(require 'chirp-x-transaction)

(defconst chirp-x-transaction-source--timeout 30
  "Maximum seconds for each preparation GET.")

(defconst chirp-x-transaction-source--body-limit (* 2 1024 1024)
  "Maximum bytes downloaded by either preparation GET.")

(defconst chirp-x-transaction-source--cache-lifetime 3600
  "Seconds for which transaction material remains fresh.")

(defvar chirp-x-transaction-source--cache nil
  "In-memory (COOKIE-HASH EXPIRATION MATERIAL) for the current cookie pair.")

(defun chirp-x-transaction-source--session (credentials user-agent)
  "Return (HASH . COOKIE) for CREDENTIALS after checking USER-AGENT.
plz writes headers into quoted curl configuration, so quotes and backslashes
must be rejected in addition to non-ASCII characters and cookie separators."
  (let ((auth-token (plist-get credentials :auth-token))
        (ct0 (plist-get credentials :ct0)))
    (dolist (value (list auth-token ct0 user-agent))
      (unless (and (stringp value) (not (string-empty-p value))
                   (not (string-match-p "[^ -~\"\\\\]" value)))
        (error "Invalid X transaction header value")))
    (when (or (string-match-p ";" auth-token) (string-match-p ";" ct0))
      (error "Invalid X transaction session cookies"))
    (cons (secure-hash 'sha256 (concat auth-token "\0" ct0))
          (format "auth_token=%s; ct0=%s" auth-token ct0))))

(defun chirp-x-transaction-source--transport-message (stage failure)
  "Describe STAGE and plz FAILURE without including response data."
  (let ((response (plz-error-response failure))
        (curl-error (plz-error-curl-error failure)))
    (format "X transaction %s retrieval failed: %s" stage
            (cond (response (format "HTTP %s" (plz-response-status response)))
                  (curl-error
                   (format "curl %s (%s)" (car curl-error)
                           (or (alist-get (car curl-error) plz-curl-errors)
                               "transfer failed")))
                  (t "transfer failed")))))

(defun chirp-x-transaction-source-prepare
    (credentials user-agent callback errback)
  "Prepare transaction material for CREDENTIALS using USER-AGENT.
CALLBACK receives the material; ERRBACK receives retrieval or parsing errors.
A fresh cookie-pair cache hit invokes CALLBACK synchronously.  Return a
cancellation function while fetching, or nil when already settled."
  (let* ((session (chirp-x-transaction-source--session credentials user-agent))
         (cookie-hash (car session))
         (cache chirp-x-transaction-source--cache))
    (if (and (equal cookie-hash (car cache))
             (< (float-time) (cadr cache)))
        (progn (funcall callback (caddr cache)) nil)
      (let ((active t) process timer)
        (cl-labels
            ((stop ()
               (setq active nil)
               (when timer
                 (cancel-timer timer)
                 (setq timer nil))
               (when (and process (process-live-p process))
                 (delete-process process)))
             (fail (message)
               (when active
                 (stop)
                 (funcall errback message)))
             (parse (function &rest arguments)
               (condition-case err
                   (apply function arguments)
                 (error
                  (fail (error-message-string err))
                  nil)))
             (get (stage url headers receive)
               (let ((plz-curl-default-args
                      (list "--disable" "--silent" "--proto" "=https"
                            "--max-redirs" "0" "--max-filesize"
                            (number-to-string chirp-x-transaction-source--body-limit))))
                 (condition-case nil
                     (setq process
                           (plz 'get url
                             :headers headers :as 'string
                             :then (lambda (body)
                                     (when active (funcall receive body)))
                             :else (lambda (failure)
                                     (fail (chirp-x-transaction-source--transport-message
                                            stage failure)))
                             :connect-timeout 5
                             :timeout chirp-x-transaction-source--timeout
                             :noquery t))
                   (error
                    (fail (format "X transaction %s retrieval could not start"
                                  stage))))))
             (script (bootstrap body)
               (when-let* ((material (parse #'chirp-x-transaction-material
                                           bootstrap body)))
                 (setq chirp-x-transaction-source--cache
                       (list cookie-hash
                             (+ (float-time) chirp-x-transaction-source--cache-lifetime)
                             material))
                 (stop)
                 (funcall callback material)))
             (home (body)
               (when-let* ((bootstrap (parse #'chirp-x-transaction-bootstrap body)))
                 ;; The parser constructs this URL from a fixed CDN prefix
                 ;; and hexadecimal chunk hash.  No cookies go to the CDN.
                 (get "script" (plist-get bootstrap :script-url)
                      `(("User-Agent" . ,user-agent) ("Accept" . "*/*"))
                      (apply-partially #'script bootstrap)))))
          ;; Bound the whole preparation even if plz's response parser fails
          ;; before delivering a callback; curl bounds each transfer itself.
          (setq timer
                (run-at-time (* 2 chirp-x-transaction-source--timeout) nil
                             (lambda () (fail "X transaction preparation timed out"))))
          (get "bootstrap" "https://x.com/home"
               `(("Cookie" . ,(cdr session))
                 ("User-Agent" . ,user-agent) ("Accept" . "text/html"))
               #'home)
          (when active #'stop))))))

(provide 'chirp-x-transaction-source)
;;; chirp-x-transaction-source.el ends here
