;;; chirp-x-transaction-test.el --- Transaction protocol tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Independent protocol vectors and numeric boundaries.  No live requests,
;; credentials, or checked-in X page fixtures.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chirp-x-transaction)

(ert-deftest chirp-x-transaction-token-golden-layout-and-hash ()
  ;; Expected token independently calculated with Python hashlib.
  (should
   (equal
    (chirp-x-transaction-id
     (list :key (apply #'unibyte-string (number-sequence 0 47))
           :animation "0123456789abcdef")
     "POST" "/i/api/graphql/query/CreateBookmark"
     (+ 1682924400 123456) 171)
    "q6uqqaivrq2so6KhoKempaS7urm4v769vLOysbC3trW0i4qJiI+OjYyDgoGAh4aFhOtJqqs8GsLIBb0Fmxmtr3QDFWP4qA")))

(ert-deftest chirp-x-transaction-javascript-decimal-and-radix-boundaries ()
  ;; Golden values from Number(x.toFixed(2)).toString(16), including binary
  ;; decimals below a midpoint and exact halfway values of either sign.
  (dolist (vector '((0.01 "0.028f5c28f5c28f6")
                    (0.1 "0.1999999999999a")
                    (0.295 "0.4a3d70a3d70a3c")
                    (0.125 "0.2147ae147ae148")
                    (-0.125 "0.2147ae147ae148")
                    (0.99 "0.fd70a3d70a3d7")
                    (0.0 "0")))
    (should (equal (chirp-x-transaction--hex
                    (chirp-x-transaction--fixed-two (car vector)))
                   (cadr vector)))))

(ert-deftest chirp-x-transaction-animation-browser-oracle ()
  ;; Official live module 88716, with the public verification/SVG fixture:
  ;; frame zero, row twelve, time (key[25]%16)*(key[40]%16)*(key[10]%16).
  ;; The compact row exercises negative eased progress, RGB rounding and a
  ;; negative rotation; no account state or full bootstrap is retained.
  (should
   (equal
    (chirp-x-transaction--animation [127 141 132 198 168 38 25 132 152 186 25]
                                    1430)
    "7d8c861007ae147ae147ae007ae147ae147ae100")))

(ert-deftest chirp-x-transaction-animation-time-rounding-and-rgb-clamping ()
  (let ((linear [0 0 0 255 255 255 0 0 127 255 255]))
    ;; JavaScript Math.round rounds a positive half upward, not to even.
    (should (equal (chirp-x-transaction--animation linear 4) "000100100"))
    (should (equal (chirp-x-transaction--animation linear 5) "111100100")))
  ;; Negative cubic progress overshoots both RGB limits.
  (should (string-prefix-p
           "0ff0"
           (chirp-x-transaction--animation [0 255 0 255 0 255 0 0 0 255 0]
                                           2048))))

(provide 'chirp-x-transaction-test)
;;; chirp-x-transaction-test.el ends here
