;;; chirp-x-transaction.el --- X client transaction tokens -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pure implementation of the public X web client's transaction protocol.
;; Algorithm references (not evaluated or embedded JavaScript):
;; https://github.com/fa0311/twitter-tid-deobf-fork/blob/main/output/additional2.js
;; https://github.com/iSarabjitDhiman/XClientTransaction
;; https://www.w3.org/TR/css-easing-1/#cubic-bezier-easing-functions
;; The verification bytes, SVG rows and key indices rotate independently.  Only
;; the fixed protocol constants belong here; unfamiliar source fails closed.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'subr-x)

(defconst chirp-x-transaction--epoch 1682924400)

(defun chirp-x-transaction--fail (message)
  "Signal a readable transaction preparation error with MESSAGE."
  (error "X transaction: %s" message))

(defun chirp-x-transaction--script-url (scripts)
  "Derive the trusted transaction chunk URL from runtime SCRIPTS."
  (let ((case-fold-search nil) urls)
    (dolist (script scripts)
      (when (string-match
             "\\.u=[[:alnum:]_$]+=>((\\({[^{}]+}\\))\\[[[:alnum:]_$]+\\]||[[:alnum:]_$]+)[+]\"\\.\"[+](\\({[^{}]+}\\))\\[[[:alnum:]_$]+\\][+]\"\\([a-f]*\\)\\.js\""
             script)
        (when (save-match-data
                (string-match "\\.u=[[:alnum:]_$]+=>" script (match-end 0)))
          (chirp-x-transaction--fail "ambiguous transaction runtime"))
        (let ((names (match-string 1 script))
              (hashes (match-string 2 script))
              (suffix (match-string 3 script)))
          (unless (string-match-p
                   "\\.p=\"https://abs\\.twimg\\.com/responsive-web/client-web/\""
                   script)
            (chirp-x-transaction--fail "untrusted runtime asset base"))
          (when (string-match
                 "\\(?:{\\|,\\)\\([0-9]+\\):\"ondemand\\.s\"\\(?:,\\|}\\)"
                 names)
            (let ((chunk (match-string 1 names)))
              (unless (string-match
                       (concat "\\(?:{\\|,\\)" chunk
                               ":\"\\([0-9a-f]\\{8,64\\}\\)\"\\(?:,\\|}\\)")
                       hashes)
                (chirp-x-transaction--fail "missing transaction chunk hash"))
              (push (concat "https://abs.twimg.com/responsive-web/client-web/ondemand.s."
                            (match-string 1 hashes) suffix ".js") urls))))))
    (unless (= (length urls) 1)
      (chirp-x-transaction--fail "missing or ambiguous transaction runtime"))
    (car urls)))

(defun chirp-x-transaction--rows (path)
  "Parse and validate sixteen animation rows in SVG PATH."
  (unless (and (stringp path) (string-prefix-p "M 10,30 C" path))
    (chirp-x-transaction--fail "malformed animation path"))
  (let ((rows (split-string (substring path 9) "C")))
    (unless (= (length rows) 16)
      (chirp-x-transaction--fail "animation must have sixteen rows"))
    (vconcat
     (mapcar
      (lambda (row)
        (unless (string-match-p
                 "\\` *[0-9]+,[0-9]+ [0-9]+,[0-9]+ [0-9]+,[0-9]+ h [0-9]+ s [0-9]+,[0-9]+ [0-9]+,[0-9]+ *\\'"
                 row)
          (chirp-x-transaction--fail "malformed animation row"))
        (let ((values (mapcar #'string-to-number
                             (split-string row "[^0-9]+" t))))
          (unless (and (= (length values) 11)
                       (cl-every (lambda (value) (<= 0 value 255)) values))
            (chirp-x-transaction--fail "animation byte out of bounds"))
          (vconcat values)))
      rows))))

(defun chirp-x-transaction-bootstrap (html)
  "Parse public transaction metadata from HTML.
Return :key (48 unibyte verification bytes), :frames (four parsed SVG row
vectors), and :script-url (a trusted static asset URL).  Never retain HTML.
Script text uses `dom-inner-text' without inserting whitespace between nodes."
  (unless (fboundp 'libxml-parse-html-region)
    (chirp-x-transaction--fail "Emacs needs built-in libxml HTML support"))
  (let* ((case-fold-search nil)
         (dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (metas (cl-remove-if-not
                 (lambda (node)
                   (equal (dom-attr node 'name) "twitter-site-verification"))
                 (dom-by-tag dom 'meta)))
         (frames (make-vector 4 nil))
         key)
    (unless (= (length metas) 1)
      (chirp-x-transaction--fail "missing or ambiguous verification key"))
    (let ((encoded (dom-attr (car metas) 'content)))
      (unless (and (stringp encoded)
                   (string-match-p "\\`[A-Za-z0-9+/]\\{64\\}\\'" encoded))
        (chirp-x-transaction--fail "malformed verification key"))
      (setq key (base64-decode-string encoded)))
    (dolist (svg (dom-by-tag dom 'svg))
      (let ((id (dom-attr svg 'id)))
        (when (and (stringp id)
                   (string-match "\\`loading-x-anim-\\([0-3]\\)\\'" id))
          (let* ((index (string-to-number (match-string 1 id)))
                 (group (car (dom-by-tag svg 'g)))
                 (paths (dom-by-tag group 'path)))
            (when (or (aref frames index) (/= (length paths) 2))
              (chirp-x-transaction--fail "ambiguous animation frame"))
            (aset frames index
                  (chirp-x-transaction--rows (dom-attr (cadr paths) 'd)))))))
    (unless (cl-every #'identity frames)
      (chirp-x-transaction--fail "missing animation frame"))
    (list :key key :frames frames
          :script-url (chirp-x-transaction--script-url
                       (mapcar #'dom-inner-text (dom-by-tag dom 'script))))))

(defun chirp-x-transaction--indices (script)
  "Extract the row index and three time indices from static SCRIPT."
  (let ((case-fold-search nil) (position 0) indices variable)
    (while (string-match
            "\\([[:alpha:]_$][[:alnum:]_$]*\\)\\[\\([0-9]+\\)\\],[ \t\n]*16\\(?:[^0-9]\\|\\'\\)"
            script position)
      (let ((name (match-string 1 script))
            (index (string-to-number (match-string 2 script))))
        (when (or (>= index 48) (and variable (not (equal name variable))))
          (chirp-x-transaction--fail "invalid dynamic key indices"))
        (setq variable name)
        (push index indices))
      (setq position (match-end 0)))
    (unless (= (length indices) 4)
      (chirp-x-transaction--fail "missing or ambiguous dynamic key indices"))
    (nreverse indices)))

(defun chirp-x-transaction--fixed-two (value)
  "Return Number(VALUE.toFixed(2)), with JavaScript binary tie rounding.
Use the exact binary significand rather than rounding VALUE * 100: that
multiplication can erase which side of a decimal midpoint VALUE lies on."
  (let* ((parts (frexp (abs (float value))))
         (mantissa (round (* (car parts) (expt 2 53))))
         (exponent (- (cdr parts) 53))
         (numerator (* mantissa 100))
         (denominator 1))
    (if (>= exponent 0)
        (setq numerator (ash numerator exponent))
      (setq denominator (ash 1 (- exponent))))
    (let* ((rounded (/ (+ (* numerator 2) denominator) (* denominator 2)))
           (decimal (format "%s%d.%02d" (if (< value 0) "-" "")
                            (/ rounded 100) (% rounded 100))))
      (string-to-number decimal))))

(defun chirp-x-transaction--hex (value)
  "Format the magnitude of finite VALUE as a JavaScript radix-16 number.
Animation values have at most two decimal places, so their exact binary
fraction terminates within fourteen hexadecimal digits."
  (let* ((magnitude (abs (float value)))
         (whole (floor magnitude))
         (fraction (- magnitude whole))
         (result (format "%x" whole))
         (count 0))
    (when (> fraction 0)
      (setq result (concat result "."))
      (while (and (> fraction 0) (< count 16))
        (setq fraction (* fraction 16))
        (let ((digit (floor fraction)))
          (setq result (concat result (format "%x" digit))
                fraction (- fraction digit)
                count (1+ count))))
      (unless (= fraction 0)
        (chirp-x-transaction--fail "nonterminating animation number")))
    result))

(defun chirp-x-transaction--cubic-coordinate (a b parameter)
  "Evaluate one cubic coordinate with controls A, B at PARAMETER."
  (let ((inverse (- 1.0 parameter)))
    (+ (* 3.0 a inverse inverse parameter)
       (* 3.0 b inverse parameter parameter)
       (* parameter parameter parameter))))

(defun chirp-x-transaction--bezier (controls time)
  "Evaluate CSS cubic-bezier CONTROLS at progress TIME."
  (let ((x1 (aref controls 0)) (y1 (aref controls 1))
        (x2 (aref controls 2)) (y2 (aref controls 3)))
    (cond
     ((<= time 0) 0.0)
     ((>= time 1) 1.0)
     (t
      (let ((low 0.0) (high 1.0) (parameter 0.5))
        ;; Bounded bisection also works for horizontal endpoint tangents.
        (dotimes (_ 52)
          (setq parameter (/ (+ low high) 2.0))
          (if (< (chirp-x-transaction--cubic-coordinate x1 x2 parameter) time)
              (setq low parameter)
            (setq high parameter)))
        (chirp-x-transaction--cubic-coordinate y1 y2 parameter))))))

(defun chirp-x-transaction--animation (row time)
  "Compute the browser's animation fingerprint from ROW at TIME milliseconds."
  (let* ((controls (make-vector 4 nil))
         (progress (/ (* 10 (floor (+ (/ time 10.0) 0.5))) 4096.0)))
    (dotimes (index 4)
      (aset controls index
            (chirp-x-transaction--fixed-two
             (+ (* (aref row (+ 7 index))
                   (/ (if (cl-oddp index) 2.0 1.0) 255))
                (if (cl-oddp index) -1 0)))))
    (let* ((eased (chirp-x-transaction--bezier controls progress))
           (angle (* (floor (+ 60 (/ (* (aref row 6) 300.0) 255))) eased))
           (radians (* angle (/ float-pi 180)))
           (cosine (cos radians)) (sine (sin radians))
           values)
      (dotimes (index 3)
        (push (floor (+ 0.5 (max 0 (min 255
                                      (+ (aref row index)
                                         (* eased (- (aref row (+ index 3))
                                                     (aref row index))))))))
              values))
      (setq values (nreverse values))
      ;; Computed CSS matrix serialization uses six significant digits before
      ;; the client's toFixed(2).  Include the translation zeros, not alpha.
      (setq values
            (append values
                    (mapcar (lambda (value)
                              (chirp-x-transaction--fixed-two
                               (string-to-number (format "%.6g" value))))
                            (list cosine sine (- sine) cosine))
                    '(0 0)))
      (string-replace "." ""
                      (mapconcat #'chirp-x-transaction--hex values "")))))

(defun chirp-x-transaction-material (bootstrap script)
  "Compute :key and :animation material from BOOTSTRAP and static SCRIPT."
  (let* ((key (plist-get bootstrap :key))
         (frames (plist-get bootstrap :frames))
         (indices (chirp-x-transaction--indices script)))
    (let* ((frame (aref frames (% (aref key 5) 4)))
           (row (aref frame (% (aref key (car indices)) 16)))
           (time (cl-reduce #'* (cdr indices)
                            :key (lambda (index) (% (aref key index) 16)))))
      (list :key key :animation (chirp-x-transaction--animation row time)))))

(defun chirp-x-transaction-id (material method path &optional unix-time random-byte)
  "Return an unpadded ASCII base64 transaction ID for METHOD and PATH.
MATERIAL comes from `chirp-x-transaction-material'.  METHOD must be uppercase;
PATH is the GraphQL pathname without a query.  UNIX-TIME is Unix seconds and
RANDOM-BYTE is an integer from 0 through 255; omitted values use current time
and a random byte.  No network activity or credential storage occurs here."
  (let* ((key (plist-get material :key))
         (animation (plist-get material :animation))
         (seconds (- (floor (or unix-time (float-time))) chirp-x-transaction--epoch))
         (mask (if (null random-byte) (random 256) random-byte))
         (digest (secure-hash
                  'sha256
                  (encode-coding-string
                   (format "%s!%s!%dobfiowerehiring%s" method path seconds animation)
                   'utf-8 t)
                  nil nil t))
         (bytes (concat (unibyte-string mask) key
                        (apply #'unibyte-string
                               (mapcar (lambda (shift) (logand (ash seconds (- shift)) 255))
                                       '(0 8 16 24)))
                        (substring digest 0 16) (unibyte-string 3))))
    (cl-loop for index from 1 below (length bytes)
             do (aset bytes index (logxor mask (aref bytes index))))
    (string-trim-right (base64-encode-string bytes t) "=+")))

(provide 'chirp-x-transaction)
;;; chirp-x-transaction.el ends here
