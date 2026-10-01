;;; chirp-translate.el --- Optional tweet translations -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep translated text in Surface-owned AppKit state, never in tweet models.

;;; Code:

(require 'appkit-translate)
(require 'map)
(require 'chirp-core)
(require 'chirp-backend)

(defcustom chirp-translation-backend-function #'chirp-translate-x-backend
  "Zero-argument factory for the tweet translation backend.
The default uses X's Grok translation endpoint.  Select
`appkit-translate-respond-backend' explicitly to use optional Respond.
The target language is shared through `appkit-translate-target-language'."
  :type '(choice (const :tag "X / Grok" chirp-translate-x-backend)
                 (const :tag "Respond" appkit-translate-respond-backend)
                 function)
  :group 'chirp)

(defun chirp-translate-x-backend ()
  "Return a backend descriptor for one authenticated X Grok translation."
  (list :id 'chirp-x-grok :label "X / Grok" :start #'chirp-translate--x-start))

(defun chirp-translate--x-start (source language resolve reject)
  "Translate SOURCE through X into LANGUAGE, calling RESOLVE or REJECT."
  (let ((request
          (chirp-backend-translate
           (map-nested-elt source '(:data :tweet-id)) language
           (lambda (data _envelope)
             (funcall resolve (map-elt data 'translation)))
           reject)))
    ;; X returns its URL retrieval buffer, not an AppKit handle.  The public
    ;; cancellation API stops that retrieval; nil permits logical revocation.
    (when (bufferp request)
      (lambda () (chirp-x-cancel-request request)))))

(defun chirp-translate-source (tweet)
  "Return the exact displayed translation source for TWEET, or nil.
The X ID is the actual revision, not the initial edit-history or repost ID."
  (map-let (:id :text :edit-history-ids) tweet
    (when id
      (list :key (list 'chirp-tweet id)
            :version (list id text edit-history-ids)
            :text (or text "")
            :data (list :tweet-id id)))))

(defun chirp-translate--notify (surface key)
  "Invalidate the visible top-level rows in SURFACE containing source KEY."
  (when (appkit-surface-live-p surface)
    (let ((buffer (appkit-surface-buffer surface)) resources)
      (with-current-buffer buffer
        (when (eq surface (appkit-current-surface))
          (chirp--map-buffer-tweets
           buffer
           (lambda (tweet)
             (let ((nested tweet) found)
               (while (and nested (not found))
                 (map-let (:id :quoted-tweet) nested
                   (setq found (equal key (list 'chirp-tweet id))
                         nested quoted-tweet)))
               (when found
                 (cl-pushnew (list 'tweet (plist-get tweet :id)) resources
                             :test #'equal)))))
          (when resources
            (appkit-surface-post
             surface
             (appkit-projection-change-create
              :full-p t
              :frame-p t
              :position 'preserve
              :resources resources))))))))
(defun chirp-translate-enable (&optional surface)
  "Enable tweet translation for Chirp SURFACE."
  (let ((surf (or surface (appkit-current-surface))))
    (when (appkit-surface-live-p surf)
      (appkit-translate-enable
       surf (lambda (key) (chirp-translate--notify surf key))))))

(defun chirp-translate-insert (tweet &optional prefix prefix-face)
  "Insert existing translation state for TWEET using PREFIX and PREFIX-FACE.
Rendering never creates a context or starts translation work."
  (when-let* ((source (chirp-translate-source tweet)))
    (appkit-translate-insert source prefix prefix-face)))

(provide 'chirp-translate)

;;; chirp-translate.el ends here
