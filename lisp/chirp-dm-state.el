;;; chirp-dm-state.el --- Canonical XChat conversation state -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Session-owned normalized XChat conversations and cross-view publication.
;; View-local history windows, requests, and composers remain outside this
;; module.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-presentation)
(require 'chirp-core)
(require 'chirp-xchat)

(defun chirp-dm-state--event-id (event)
  "Return normalized EVENT's stable identity."
  (map-let (:sequence-id :id) event
    (or sequence-id id
        (error "XChat event has no stable identity"))))

(defun chirp-dm-state-merge-events (current fetched)
  "Merge ordered XChat event lists CURRENT and FETCHED by stable identity.

When both lists contain one event, preserve CURRENT so verified plaintext is
not replaced by a later encrypted snapshot."
  (let (merged)
    (while (and current fetched)
      (let* ((left (car current))
             (right (car fetched))
             (left-id (chirp-dm-state--event-id left))
             (right-id (chirp-dm-state--event-id right)))
        (cond
         ((equal left-id right-id)
          (push left merged)
          (setq current (cdr current)
                fetched (cdr fetched)))
         ((chirp-xchat-event-before-p left right)
          (push left merged)
          (setq current (cdr current)))
         (t
          (push right merged)
          (setq fetched (cdr fetched))))))
    (nconc (nreverse merged) current fetched)))

(defun chirp-dm-state--reaction-event-p (event)
  "Return non-nil when EVENT adds or removes a message reaction."
  (memq (plist-get event :content-kind) '(reaction reaction-removed)))

(defun chirp-dm-state-visible-events (events)
  "Return ordered EVENTS excluding message-targeted reaction operations."
  (cl-remove-if #'chirp-dm-state--reaction-event-p events))

(defun chirp-dm-state--apply-reaction-event (target event)
  "Apply one normalized reaction EVENT to its TARGET message."
  (pcase-let* (((map (:text emoji) :sender-id :content-kind) event)
              (reactions (map-elt target :reactions))
         (reaction
          (cl-find emoji reactions
                   :key (lambda (item) (plist-get item :emoji))
                   :test #'equal)))
    (when (and (stringp emoji) (not (string-empty-p emoji))
               (stringp sender-id) (not (string-empty-p sender-id)))
      (unless reaction
        (setq reaction (list :emoji emoji :count 0 :senders nil)
              reactions (append reactions (list reaction))))
      (if (eq content-kind 'reaction)
          (cl-pushnew sender-id (plist-get reaction :senders) :test #'equal)
        (setf (plist-get reaction :senders)
              (delete sender-id (map-elt reaction :senders))))
      (let ((senders (map-elt reaction :senders)))
        (if senders
            (setf (plist-get reaction :count) (length senders))
          (setq reactions (delq reaction reactions))))
      (setf (plist-get target :reactions) reactions))))

(defun chirp-dm-state--refresh-reactions (events)
  "Rebuild target-message reaction aggregates from ordered EVENTS."
  (let ((targets (make-hash-table :test #'equal))
        (events
         (mapcar
          (lambda (event) (plist-put event :reactions nil))
          events)))
    (dolist (event events)
      (unless (chirp-dm-state--reaction-event-p event)
        (map-let (:sequence-id :id) event
          (dolist (identity (delete-dups (delq nil (list sequence-id id))))
            (puthash identity event targets)))))
    (dolist (event events)
      (when-let* (((chirp-dm-state--reaction-event-p event))
                  (target-id (plist-get event :target-message-id))
                  (target (gethash target-id targets)))
        (chirp-dm-state--apply-reaction-event target event)))
    events))

(defun chirp-dm-state--refresh-derived-fields (conversation)
  "Refresh event-derived fields on canonical CONVERSATION."
  (let* ((events (plist-get conversation :events))
         (activity (car (last events)))
         (latest
          (car (last (cl-remove-if
                      #'chirp-dm-state--reaction-event-p events)))))
    (setf (plist-get conversation :latest-event) latest
          (plist-get conversation :preview)
          (and latest (chirp-xchat-event-label latest))
          (plist-get conversation :updated-at-msec)
          (and activity (plist-get activity :created-at-msec))))
  conversation)

(defun chirp-dm-state--set-events (conversation events)
  "Replace canonical CONVERSATION's ordered EVENTS and derived fields."
  (setf (plist-get conversation :events)
        (chirp-dm-state--refresh-reactions events))
  (chirp-dm-state--refresh-derived-fields conversation))

(defun chirp-dm-state--commit (operation &rest arguments)
  "Synchronously commit OPERATION with ARGUMENTS under the current App."
  (let* ((app (or chirp--app (chirp-app)))
         (id (plist-get (car arguments)
                        (if (eq operation 'live-event) :conversation-id :id))))
    (appkit-app-send app (list 'chirp-dm-state operation arguments))
    (gethash id (chirp--session-dm-conversations (appkit-app-model app)))))

(defun chirp-dm-state-set-events (conversation events)
  "Commit ordered EVENTS and derived fields to canonical CONVERSATION."
  (chirp-dm-state--commit 'set-events conversation events))

(defun chirp-dm-state-accept-live-event (event)
  "Commit normalized websocket EVENT and notify its interested Surfaces.
Return its canonical conversation, or nil if metadata has not been loaded."
  (when-let* ((conversation (chirp-dm-state--commit 'live-event event)))
    (chirp-dm-state-publish
     conversation (chirp-dm-state--event-id event) t t)
    conversation))

(defun chirp-dm-state--copy-field (target source property)
  "Copy PROPERTY from SOURCE to TARGET when SOURCE carries it."
  (when (plist-member source property)
    (setf (plist-get target property)
          (copy-tree (plist-get source property)))))

(cl-defun chirp-dm-state--merge-snapshot
    (conversation snapshot &key events)
  "Merge continuity-checked SNAPSHOT and optional EVENTS into CONVERSATION."
  (dolist (property '(:type :title :participants :muted-p
                      :message-request-p :has-more :older-cursor))
    (chirp-dm-state--copy-field conversation snapshot property))
  (chirp-dm-state--set-events
   conversation
   (or events
       (chirp-dm-state-merge-events
        (plist-get conversation :events)
        (plist-get snapshot :events))))
  conversation)

(cl-defun chirp-dm-state-merge-snapshot
    (conversation snapshot &key events)
  "Commit continuity-checked SNAPSHOT and optional EVENTS to CONVERSATION."
  (chirp-dm-state--commit 'merge-snapshot conversation snapshot events))

(defun chirp-dm-state--acquire (table snapshot)
  "Return TABLE's canonical conversation for continuity-checked SNAPSHOT."
  (let ((id (and (listp snapshot) (plist-get snapshot :id))))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "XChat conversation has no canonical identity"))
    (let ((conversation (gethash id table)))
      (cond
       ((eq conversation snapshot))
       (conversation
        (chirp-dm-state--merge-snapshot conversation snapshot))
       (t
        (setq conversation (plist-put (copy-tree snapshot) :inbox-preview nil))
        (chirp-dm-state--set-events
         conversation (plist-get conversation :events))
        (puthash id conversation table)))
      conversation)))

(defun chirp-dm-state-acquire (snapshot)
  "Return the App-owned canonical conversation for checked SNAPSHOT."
  (chirp-dm-state--commit 'acquire snapshot))

(defun chirp-dm-state--acquire-preview (table snapshot)
  "Acquire inbox SNAPSHOT in TABLE without joining disjoint timeline fragments."
  (pcase-let* (((map :id (:events fetched)) snapshot)
               (conversation (gethash id table))
               (current (map-elt conversation :events)))
    (if (or (null conversation) (null current))
        (chirp-dm-state--acquire table snapshot)
      ;; Inbox metadata must not replace an open timeline's older-page cursor.
      (dolist (property '(:type :title :participants :muted-p :message-request-p))
        (chirp-dm-state--copy-field conversation snapshot property))
      (if (cl-some
           (lambda (event)
             (cl-find (chirp-dm-state--event-id event) current
                      :key #'chirp-dm-state--event-id :test #'equal))
           fetched)
          (chirp-dm-state--set-events
           conversation (chirp-dm-state-merge-events current fetched))
        ;; A disjoint bounded preview is not evidence of timeline continuity.
        (pcase-let* (((map :latest-event :preview :updated-at-msec) snapshot)
                     (previous (map-elt conversation :inbox-preview))
                     ((map :kind :sequence-id :sender-id) latest-event))
          (when (chirp-xchat-event-before-p
                 (map-elt previous :latest-event) latest-event)
            (setf (plist-get conversation :inbox-preview)
                  (list :preview (copy-tree preview)
                        :updated-at-msec updated-at-msec
                        :latest-event
                        (list :kind kind
                              :sequence-id sequence-id
                              :sender-id sender-id))))))
      conversation)))

(defun chirp-dm-state-acquire-preview (snapshot)
  "Commit bounded inbox SNAPSHOT without bypassing history continuity."
  (let* ((conversation (chirp-dm-state--commit 'acquire-preview snapshot))
         (first (car (plist-get conversation :events))))
    (chirp-dm-state-publish
     conversation (and first (chirp-dm-state--event-id first)) nil)
    conversation))

(defun chirp-dm-state--app-update (_context session message)
  "Apply canonical DM MESSAGE to its original SESSION."
  (let* ((table (chirp--session-dm-conversations session))
         (arguments (nth 2 message))
         (conversation (car arguments)))
    (pcase (nth 1 message)
      ('set-events (apply #'chirp-dm-state--set-events arguments))
      ('merge-snapshot
       (chirp-dm-state--merge-snapshot
        conversation (nth 1 arguments) :events (nth 2 arguments)))
      ('acquire (chirp-dm-state--acquire table conversation))
      ('acquire-preview (chirp-dm-state--acquire-preview table conversation))
      ('live-event
       (let* ((event conversation)
              (canonical (gethash (plist-get event :conversation-id) table)))
         (when canonical
           (chirp-dm-state--set-events
            canonical
            (chirp-dm-state-merge-events
             (plist-get canonical :events) (list event))))))
      (_ (error "Unsupported canonical DM operation: %S" message)))
    (appkit-next :model session :render appkit-render-none)))

(defun chirp-dm-state-publish
    (conversation &optional seed-key decrypt-p promote-p)
  "Notify exact live Surfaces of committed CONVERSATION.
SEED-KEY may seed an authoritative-empty window.  DECRYPT-P requests pending
verification; PROMOTE-P moves live activity to the inbox's recent edge.
Only Surface reducers change their own local state."
  (when (appkit-app-live-p chirp--app)
    (dolist (view (appkit-app--surface-snapshot chirp--app))
      (when (appkit-surface-live-p view)
        (pcase-let* ((state (appkit-surface-model view))
                     ((map :type (:conversation current) :items) state))
          (pcase type
            ('dm-conversation
             (when (eq current conversation)
               (appkit-surface-post
                view (list 'chirp-dm-conversation 'canonical-changed
                           view conversation seed-key decrypt-p))))
            ('dm-inbox
             (when (or promote-p (memq conversation items))
               (appkit-surface-post
                view (list 'chirp-dm-inbox 'canonical-changed
                           state conversation promote-p))))))))))

(provide 'chirp-dm-state)

;;; chirp-dm-state.el ends here
