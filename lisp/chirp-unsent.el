;;; chirp-unsent.el --- Unsent draft and scheduled post lists -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Present X-server drafts and scheduled posts in a tabulated list.  Marks
;; follow Buffer Menu conventions: `d' flags deletion and `x' executes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'appkit-evil)
(require 'chirp-core)
(require 'chirp-backend)
(require 'chirp-actions)

;;; Constants

(defconst chirp-unsent--mark-char ?>
  "Character used to mark an unsent row.")

(defconst chirp-unsent--delete-char ?D
  "Character used to flag an unsent row for deletion.")

;;; Variables

(defvar-local chirp-unsent-kind 'draft
  "Unsent collection shown in the current buffer.

Either `draft' or `scheduled'.")

(defvar-local chirp-unsent-entries nil
  "Normalized unsent entries last rendered in the current buffer.")

(defvar-local chirp-unsent--collection nil
  "Collection incarnation, replaced on kind navigation and mode initialization.")

(defvar-local chirp-unsent--fetch-token nil
  "Identity of the outstanding collection fetch.")

(defvar-local chirp-unsent--fetch-results nil
  "Confirmed local results to overlay on the outstanding fetch.")

(defvar-local chirp-unsent--status-owner nil
  "Operation permitted to clear this controller's current status.")

(cl-defstruct (chirp-unsent--source
               (:constructor chirp-unsent--make-source))
  buffer collection kind)

(defun chirp-unsent-capture-source (buffer)
  "Capture BUFFER's exact unsent controller and collection, or return nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'chirp-unsent-mode)
        (chirp-unsent--make-source
         :buffer buffer
         :collection chirp-unsent--collection :kind chirp-unsent-kind)))))

(defun chirp-unsent--source-current-p (source)
  "Return non-nil if SOURCE still owns its original collection."
  (and source
       (buffer-live-p (chirp-unsent--source-buffer source))
       (with-current-buffer (chirp-unsent--source-buffer source)
         (and (derived-mode-p 'chirp-unsent-mode)
              (eq chirp-unsent--collection
                  (chirp-unsent--source-collection source))))))

(defun chirp-unsent--clear-status (source owner)
  "Clear status only when SOURCE and its status OWNER remain current."
  (when (chirp-unsent--source-current-p source)
    (with-current-buffer (chirp-unsent--source-buffer source)
      (when (eq owner chirp-unsent--status-owner)
        (setq chirp-unsent--status-owner nil)
        (chirp-clear-status)))))

;;; Mode

(defvar-keymap chirp-unsent-mode-map
  :doc "Keymap for `chirp-unsent-mode'."
  :parent tabulated-list-mode-map
  "RET" #'chirp-unsent-open
  "g" #'chirp-unsent-refresh
  "m" #'chirp-unsent-mark
  "u" #'chirp-unsent-unmark
  "U" #'chirp-unsent-unmark-all
  "d" #'chirp-unsent-flag-delete
  "x" #'chirp-unsent-execute
  "TAB" #'chirp-unsent-toggle-kind
  "q" #'chirp-quit-current-buffer)

;;; Rows

(defun chirp-unsent--when-label (entry)
  "Return the When column label for ENTRY."
  (if-let* ((execute-at (plist-get entry :execute-at)))
      (format-time-string "%Y-%m-%d %H:%M" execute-at)
    (if (eq (plist-get entry :kind) 'scheduled)
        "Scheduled"
      "Draft")))

(defun chirp-unsent--preview (entry)
  "Return the Text column value for ENTRY."
  (let* ((text (string-replace "\n" " " (or (car (plist-get entry :texts)) "")))
         (media (or (plist-get entry :media-count) 0))
         (prefix (if (> media 0)
                     (format "[%d img] " media)
                   "")))
    (truncate-string-to-width (concat prefix text) 80 nil nil "…")))

(defun chirp-unsent--row (entry)
  "Return a tabulated-list row for ENTRY."
  (list (plist-get entry :id)
        (vector " "
                (format "%d" (length (plist-get entry :texts)))
                (chirp-unsent--preview entry)
                (chirp-unsent--when-label entry))))

(defun chirp-unsent--title ()
  "Return the buffer title for the current unsent collection."
  (if (eq chirp-unsent-kind 'scheduled)
      "Scheduled"
    "Drafts"))

(define-derived-mode chirp-unsent-mode tabulated-list-mode "Chirp-Unsent"
  "Major mode for X drafts and scheduled posts."
  (setq-local chirp-unsent--collection (make-symbol "unsent-collection"))
  ;; Tabulated List entries are permanent-local; a new controller must not
  ;; inherit actionable rows from the previous incarnation.
  (setq-local tabulated-list-entries nil)
  (setq-local truncate-lines t)
  (setq-local tabulated-list-format
              [("C" 1 nil :pad-right 1)
               ("Posts" 5 t :right-align t)
               ("Text" 80 t)
               ("When" 16 t :right-align t)])
  (setq-local tabulated-list-padding 0)
  (setq-local tabulated-list-sort-key nil)
  (setq-local mode-line-process
              '((:eval (chirp--mode-line-status-string))))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (appkit-evil-normalize-keymaps))

(defun chirp-unsent--setup-evil ()
  "Install optional Evil bindings for unsent draft lists."
  (when appkit-evil-enable-integration
    (appkit-evil-set-initial-states '(chirp-unsent-mode) 'normal)
    (appkit-evil-define-readonly-keys 'chirp-unsent-mode-map)
    (appkit-evil-map
      :map chirp-unsent-mode-map
      :nm
      "RET" #'chirp-unsent-open
      "g r" #'chirp-unsent-refresh
      "M" #'chirp-unsent-mark
      "U" #'chirp-unsent-unmark
      "g U" #'chirp-unsent-unmark-all
      "D" #'chirp-unsent-flag-delete
      "X" #'chirp-unsent-execute
      "TAB" #'chirp-unsent-toggle-kind)
    (appkit-evil-normalize-buffers '(chirp-unsent-mode))))

(chirp-unsent--setup-evil)

(with-eval-after-load 'evil
  (chirp-unsent--setup-evil))

(defun chirp-unsent--buffer ()
  "Return the reusable unsent list buffer."
  (or (cl-find-if (lambda (buffer)
                    (with-current-buffer buffer
                      (derived-mode-p 'chirp-unsent-mode)))
                  (buffer-list))
      (generate-new-buffer (chirp--format-buffer-name "Drafts"))))

(defun chirp-unsent--apply-entries (entries &optional preserve-marks)
  "Render committed ENTRIES, retaining row marks when PRESERVE-MARKS is set."
  (let ((old-rows (and preserve-marks tabulated-list-entries)))
    (setq-local chirp-unsent-entries entries)
    (setq-local tabulated-list-entries
                (mapcar
                 (lambda (entry)
                   (let ((row (chirp-unsent--row entry)))
                     (when-let* ((old (assoc (car row) old-rows)))
                       (aset (cadr row) 0 (aref (cadr old) 0)))
                     row))
                 entries)))
  (tabulated-list-print t)
  (chirp--apply-buffer-name (current-buffer) (chirp-unsent--title)))

(defun chirp-unsent--merge-result (entries id entry)
  "Return ENTRIES with ID replaced by ENTRY, inserted, or deleted if nil."
  (let (found result)
    (dolist (old entries)
      (if (equal id (plist-get old :id))
          (progn
            (setq found t)
            (when entry (push entry result)))
        (push old result)))
    (setq result (nreverse result))
    (if (and entry (not found))
        (cons entry result)
      result)))

(defun chirp-unsent-apply-result (source kind id &optional entry)
  "Apply a confirmed unsent write to SOURCE without fetching.
KIND and ID identify the object.  ENTRY is a normalized unsent entry to
upsert; nil means confirmed deletion.  Ignore a replaced controller or
collection.  Retain unrelated marks and point."
  (when (and (chirp-unsent--source-current-p source)
             (eq kind (chirp-unsent--source-kind source)))
    (with-current-buffer (chirp-unsent--source-buffer source)
      ;; A fetch issued before this acknowledged write must not resurrect or
      ;; overwrite it.  Keep only the newest result for each ID.
      (when chirp-unsent--fetch-token
        (setf (alist-get id chirp-unsent--fetch-results nil nil #'equal) entry))
      (chirp-unsent--apply-entries
       (chirp-unsent--merge-result chirp-unsent-entries id entry) t))))

;;; Requests

(defun chirp-unsent-refresh ()
  "Reload the current unsent collection from X."
  (interactive)
  (unless (derived-mode-p 'chirp-unsent-mode)
    (user-error "Not in a Chirp unsent buffer"))
  (let* ((buffer (current-buffer))
         (source (chirp-unsent-capture-source buffer))
         (kind chirp-unsent-kind)
         (token (make-symbol "unsent-fetch")))
    (setq chirp-unsent--fetch-token token
          chirp-unsent--fetch-results nil
          chirp-unsent--status-owner token)
    (chirp-set-status buffer (format "Loading %s..." (chirp-unsent--title)))
    (chirp-backend-fetch-unsent
     kind
     (lambda (entries _envelope)
       (when (chirp-unsent--source-current-p source)
         (with-current-buffer buffer
           (when (eq chirp-unsent--fetch-token token)
             (dolist (result chirp-unsent--fetch-results)
               (setq entries (chirp-unsent--merge-result
                              entries (car result) (cdr result))))
             (setq chirp-unsent--fetch-token nil
                   chirp-unsent--fetch-results nil)
             (chirp-unsent--clear-status source token)
             (chirp-unsent--apply-entries entries t)
             (message "%s: %d" (chirp-unsent--title) (length entries))))))
     (lambda (message)
       (when (chirp-unsent--source-current-p source)
         (with-current-buffer buffer
           (when (eq chirp-unsent--fetch-token token)
             (setq chirp-unsent--fetch-token nil
                   chirp-unsent--fetch-results nil)
             (chirp-unsent--clear-status source token)
             (chirp-actions-show-error message))))))))

;;; Commands

(defun chirp-unsent-open-kind (kind)
  "Open the unsent list for KIND.

KIND is `draft' or `scheduled'."
  (unless (memq kind '(draft scheduled))
    (error "Unsent kind is invalid: %S" kind))
  (let ((buffer (chirp-unsent--buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'chirp-unsent-mode)
        (chirp-unsent-mode))
      (unless (eq chirp-unsent-kind kind)
        ;; Change identity and actionable rows in one synchronous transition.
        (setq chirp-unsent-kind kind
              chirp-unsent--collection (make-symbol "unsent-collection")
              chirp-unsent--fetch-token nil
              chirp-unsent--fetch-results nil)
        (chirp-unsent--apply-entries nil))
      (chirp-unsent-refresh))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun chirp-unsent-drafts ()
  "Open the authenticated account's X drafts."
  (interactive)
  (chirp-unsent-open-kind 'draft))

;;;###autoload
(defun chirp-unsent-scheduled ()
  "Open the authenticated account's scheduled posts."
  (interactive)
  (chirp-unsent-open-kind 'scheduled))

(defun chirp-unsent-toggle-kind ()
  "Switch the current unsent buffer between drafts and scheduled posts."
  (interactive)
  (unless (derived-mode-p 'chirp-unsent-mode)
    (user-error "Not in a Chirp unsent buffer"))
  (chirp-unsent-open-kind
   (if (eq chirp-unsent-kind 'scheduled) 'draft 'scheduled)))

(defun chirp-unsent--entry-at-point ()
  "Return the normalized unsent entry at point."
  (let ((id (tabulated-list-get-id)))
    (or (cl-find-if (lambda (entry)
                      (equal (plist-get entry :id) id))
                    chirp-unsent-entries)
        (user-error "No unsent post at point"))))

(defun chirp-unsent-open ()
  "Open the unsent post at point in a compose buffer."
  (interactive)
  (chirp-compose-open-unsent (chirp-unsent--entry-at-point)))

;;; Marks and Deletion

(defun chirp-unsent--set-mark (char)
  "Put CHAR on the current unsent row and move down."
  (unless (tabulated-list-get-id)
    (user-error "No unsent post at point"))
  (tabulated-list-set-col 0 (char-to-string char) t)
  (forward-line 1))

(defun chirp-unsent-mark ()
  "Mark the unsent post at point."
  (interactive)
  (chirp-unsent--set-mark chirp-unsent--mark-char))

(defun chirp-unsent-flag-delete ()
  "Flag the unsent post at point for deletion."
  (interactive)
  (chirp-unsent--set-mark chirp-unsent--delete-char))

(defun chirp-unsent-unmark ()
  "Remove the mark from the unsent post at point."
  (interactive)
  (unless (tabulated-list-get-id)
    (user-error "No unsent post at point"))
  (tabulated-list-set-col 0 " " t)
  (forward-line 1))

(defun chirp-unsent-unmark-all ()
  "Remove every mark in the current unsent list."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (tabulated-list-get-id)
        (tabulated-list-set-col 0 " " t))
      (forward-line 1))))

(defun chirp-unsent--flagged-ids (char)
  "Return IDs whose first column is CHAR."
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((entry (tabulated-list-get-entry))
                    (mark (aref entry 0))
                    ((and (stringp mark)
                          (not (string-empty-p mark))
                          (eq (aref mark 0) char))))
          (push (tabulated-list-get-id) ids))
        (forward-line 1)))
    (nreverse ids)))

(defun chirp-unsent--delete-ids (kind ids source owner)
  "Delete approved IDS sequentially using their captured KIND.
The approved server work may finish after navigation, but only SOURCE
may receive local results; OWNER may clear only its own status."
  (if (null ids)
      (chirp-unsent--clear-status source owner)
    (chirp-backend-delete-unsent
     kind (car ids)
     (lambda (_payload _envelope)
       (chirp-unsent-apply-result source kind (car ids))
       (chirp-unsent--delete-ids kind (cdr ids) source owner))
     (lambda (message)
       (when (chirp-unsent--source-current-p source)
         (chirp-unsent--clear-status source owner)
         (chirp-actions-show-error message))))))

(defun chirp-unsent-execute ()
  "Delete every unsent post flagged with `chirp-unsent-flag-delete'."
  (interactive)
  (unless (derived-mode-p 'chirp-unsent-mode)
    (user-error "Not in a Chirp unsent buffer"))
  (let* ((ids (chirp-unsent--flagged-ids chirp-unsent--delete-char))
         (kind chirp-unsent-kind)
         (label (if (eq kind 'scheduled) "scheduled post" "draft"))
         (buffer (current-buffer))
         (source (chirp-unsent-capture-source buffer))
         (owner (make-symbol "unsent-delete")))
    (when (null ids)
      (user-error "No unsent posts flagged for deletion"))
    (unless (yes-or-no-p
             (format "Delete %d %s? "
                     (length ids)
                     (if (= (length ids) 1)
                         label
                       (if (eq kind 'scheduled)
                           "scheduled posts"
                         "drafts"))))
      (user-error "Delete canceled"))
    (when (chirp-unsent--source-current-p source)
      (with-current-buffer buffer
        (setq chirp-unsent--status-owner owner)
        (chirp-set-status buffer "Deleting...")))
    (chirp-unsent--delete-ids kind ids source owner)))

(provide 'chirp-unsent)

;;; chirp-unsent.el ends here
