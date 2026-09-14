;;; chirp-spam.el --- Spam rule storage and matching -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Literal rule storage and matching, independent of conversation visibility.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'chirp-spam-rules)

(defcustom chirp-spam-rules
  (copy-tree chirp-spam-rules-default)
  "Keywords used to hide replies in thread views.

Each nonempty string is matched literally and case-insensitively against reply
text, expanded URLs, the author's display name, and the author's handle.
Whitespace is collapsed in both rules and fields; phrases do not cross fields.  A
nested list matches only when all of its strings are nonempty and occur, which
lets specific split templates avoid broad single-keyword matches.  The
conservative defaults come from repeated spam in real public replies, with
Chinese patterns prioritized over English ones.  The thread's focus tweet is
never filtered.  Set this option to nil to disable keyword filtering, or
replace and extend the list with local patterns."
  :type '(repeat (choice string (repeat string)))
  :group 'chirp)

(defcustom chirp-spam-rules-file
  (locate-user-emacs-file "chirp/spam-rules.txt")
  "File containing persistent user spam phrases and keywords.

Store one literal rule per line.  Empty lines and lines beginning with `#' are
ignored.  These rules share the same case-insensitive match scope as
`chirp-spam-rules': reply text, expanded URLs, author display names,
and author handles."
  :type 'file
  :group 'chirp)

(defun chirp-spam-normalize (rule)
  "Return RULE as one trimmed line, or nil when it is empty."
  (when (stringp rule)
    (let ((normalized
           (string-trim
            (replace-regexp-in-string "[[:space:]]+" " " rule))))
      (unless (string-empty-p normalized)
        normalized))))

(defun chirp-spam--rule-present-p (rule rules)
  "Return non-nil when literal RULE already occurs in RULES ignoring case."
  (when-let* ((key (chirp-spam-normalize rule)))
    (setq key (downcase key))
    (cl-some
     (lambda (candidate)
       (and (stringp candidate)
            (equal key
                   (downcase
                    (or (chirp-spam-normalize candidate) "")))))
     rules)))

(defun chirp-spam--read-user-rules ()
  "Return literal spam rules read from `chirp-spam-rules-file'."
  (when (and (stringp chirp-spam-rules-file)
             (file-readable-p chirp-spam-rules-file)
             (not (file-directory-p chirp-spam-rules-file)))
    (with-temp-buffer
      (insert-file-contents chirp-spam-rules-file)
      (let ((seen (make-hash-table :test #'equal))
            rules)
        (dolist (line (split-string (buffer-string) "\n"))
          (when-let* ((rule (chirp-spam-normalize line))
                      ((not (string-prefix-p "#" rule)))
                      (key (downcase rule))
                      ((not (gethash key seen))))
            (puthash key t seen)
            (push rule rules)))
        (nreverse rules)))))

(defun chirp-spam-effective-rules ()
  "Return built-in, customized, and persistent literal spam rules."
  (let ((rules (copy-tree chirp-spam-rules)))
    (dolist (rule (chirp-spam--read-user-rules) rules)
      (unless (chirp-spam--rule-present-p rule rules)
        (setq rules (append rules (list rule)))))))

(defun chirp-spam-add-rule (input)
  "Persist normalized INPUT, returning the rule if added or nil if duplicate."
  (let ((rule (chirp-spam-normalize input)))
    (unless rule (user-error "Spam rule cannot be empty"))
    (when (string-prefix-p "#" rule)
      (user-error "Spam rule cannot begin with #"))
    (unless (and (stringp chirp-spam-rules-file)
                 (not (string-empty-p chirp-spam-rules-file)))
      (user-error "No user spam rules file is configured"))
    (unless (chirp-spam--rule-present-p rule (chirp-spam-effective-rules))
      (chirp-spam--append-user-rule rule)
      rule)))

(defun chirp-spam--append-user-rule (rule)
  "Append literal RULE to `chirp-spam-rules-file'."
  (let* ((file (expand-file-name chirp-spam-rules-file))
         (directory (file-name-directory file))
         (needs-newline
          (and (file-readable-p file)
               (> (file-attribute-size (file-attributes file)) 0)
               (with-temp-buffer
                 (insert-file-contents file)
                 (not (eq (char-before (point-max)) ?\n))))))
    (make-directory directory t)
    (with-temp-buffer
      (set-buffer-file-coding-system 'utf-8-unix)
      (when needs-newline
        (insert "\n"))
      (insert rule "\n")
      (write-region (point-min) (point-max) file t 'silent))))

(defun chirp-spam-match-p (tweet rules)
  "Return non-nil when TWEET content or author matches explicit RULES.
Normalize whitespace in both fields and rules.  A phrase stays within one
field, while grouped fragments may match different fields.  Empty RULES
never match; conversation visibility is the caller's policy."
  (let ((case-fold-search t)
        (fields (delq nil
                      (mapcar #'chirp-spam-normalize
                              (append (list (plist-get tweet :text)
                                            (plist-get tweet :author-name)
                                            (plist-get tweet :author-handle))
                                      (plist-get tweet :urls))))))
    (cl-labels ((matches (keyword)
                  (when-let* ((normalized (chirp-spam-normalize keyword)))
                    (let ((pattern (regexp-quote normalized)))
                      (cl-some (lambda (field) (string-match-p pattern field))
                               fields)))))
      (cl-some (lambda (rule)
                 (if (listp rule)
                     (and rule (cl-every #'matches rule))
                   (matches rule)))
               rules))))

(provide 'chirp-spam)
;;; chirp-spam.el ends here
