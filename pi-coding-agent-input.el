;;; pi-coding-agent-input.el --- Input buffer, history, and completion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Input buffer features for pi-coding-agent: prompt composition,
;; history navigation (comint/eshell-style M-p/M-n), incremental
;; history search (readline-style C-r), file reference completion (@),
;; path completion (Tab), slash command completion, message queuing
;; (follow-up and steering), and send/abort commands.
;;
;; Key entry points:
;;   `pi-coding-agent-send'                  Send prompt (C-c C-c)
;;   `pi-coding-agent-abort'                 Abort current operation (C-c C-k)
;;   `pi-coding-agent-quit'                  Close session
;;   `pi-coding-agent-previous-input'        History backward (M-p)
;;   `pi-coding-agent-next-input'            History forward (M-n)
;;   `pi-coding-agent-input-previous-message' Navigate previous chat message
;;   `pi-coding-agent-input-next-message'    Navigate next chat message
;;   `pi-coding-agent-history-isearch-backward'  History search (C-r)
;;   `pi-coding-agent-queue-steering'        Steering message (C-c C-s)

;;; Code:

(require 'pi-coding-agent-render)
(require 'cl-lib)
(require 'ring)

(declare-function cabins-agent-workspace-open-temporary-workbench
                  "cabins-agent-workspace" (buffer))

;;;; Image Attachments

(defcustom pi-coding-agent-image-max-width 2000
  "Maximum normalized image width in pixels."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-image-max-height 2000
  "Maximum normalized image height in pixels."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-image-max-base64-bytes (* 9 512 1024)
  "Maximum base64 payload size for one attached image."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-image-message-max-count 4
  "Maximum number of image attachments accepted in one message."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-image-message-max-base64-bytes (* 12 1024 1024)
  "Maximum aggregate base64 payload size for one message."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-image-capture-function
  #'pi-coding-agent--capture-clipboard-image
  "Function which writes a clipboard image into FILE.
It returns `no-image' when the clipboard has no supported image, nil on
success, or signals an error for a capture failure."
  :type 'function
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-image-normalize-function
  #'pi-coding-agent--normalize-image
  "Function which normalizes SOURCE into DESTINATION and returns its metadata."
  :type 'function
  :group 'pi-coding-agent)

(defconst pi-coding-agent--image-token-regexp
  "\\[\\[pi-image:\\([[:alnum:]-]+\\)\\]\\]"
  "Regexp matching the canonical attachment token syntax.")

(defvar-local pi-coding-agent--image-attachments nil
  "Hash table mapping attachment UUIDs to client-owned metadata plists.")

(defvar-local pi-coding-agent--image-attachment-directory nil
  "Directory containing normalized image bytes owned by this input buffer.")

(defvar-local pi-coding-agent--image-pending-send nil
  "Non-nil while a model-capability lookup owns a pending image send.")

(defvar-local pi-coding-agent--image-send-generation 0
  "Monotonic generation for capability-gated image sends.")

(defun pi-coding-agent--image-registry ()
  "Return the current input buffer's image registry, creating it if needed."
  (unless (hash-table-p pi-coding-agent--image-attachments)
    (setq pi-coding-agent--image-attachments (make-hash-table :test #'equal)))
  pi-coding-agent--image-attachments)

(defun pi-coding-agent--image-directory ()
  "Return a dedicated temporary directory for this input buffer."
  (unless (and pi-coding-agent--image-attachment-directory
               (file-directory-p pi-coding-agent--image-attachment-directory))
    (setq pi-coding-agent--image-attachment-directory
          (make-temp-file "pi-coding-agent-images-" t)))
  pi-coding-agent--image-attachment-directory)

(defun pi-coding-agent--image-record (uuid)
  "Return the current buffer attachment record for UUID."
  (gethash uuid (pi-coding-agent--image-registry)))

(defun pi-coding-agent--image-token (uuid)
  "Return the canonical textual image token for UUID."
  (format "[[pi-image:%s]]" uuid))

(defun pi-coding-agent--image-record-live-p (record)
  "Return non-nil when RECORD still has readable backing bytes."
  (and record
       (stringp (plist-get record :path))
       (file-readable-p (plist-get record :path))))

(defun pi-coding-agent--image-status ()
  "Return the current chat model's image capability state."
  (let* ((chat (pi-coding-agent--get-chat-buffer))
         (model (and (buffer-live-p chat)
                     (buffer-local-value 'pi-coding-agent--state chat)))
         (input (and model (plist-get (plist-get model :model) :input))))
    (cond
     ((or (null model) (null input)) 'unknown)
     ((member "image" (if (vectorp input) (append input nil) input)) 'enabled)
     (t 'unsupported))))

(defun pi-coding-agent--resolve-image-capability-and-resend (chat-buf &optional action)
  "Resolve CHAT-BUF model capability, then retry unchanged draft ACTION.
ACTION defaults to `send' and may also be `steer'."
  (if pi-coding-agent--image-pending-send
      (message "Pi: Image capability resolution is still pending")
    (let* ((input-buf (current-buffer))
           (proc (and (buffer-live-p chat-buf)
                      (with-current-buffer chat-buf
                        (pi-coding-agent--get-process))))
           (tick (buffer-chars-modified-tick))
           (transition (and (buffer-live-p chat-buf)
                            (with-current-buffer chat-buf
                              pi-coding-agent--session-transition-generation)))
           (generation (cl-incf pi-coding-agent--image-send-generation)))
      (unless proc
        (user-error "Pi: No live session is available to resolve image capability"))
      (setq pi-coding-agent--image-pending-send
            (list :generation generation :tick tick :transition transition
                  :action (or action 'send)))
      (pi-coding-agent--rpc-async
       proc (list :type "get_state")
       (lambda (response)
         (when (buffer-live-p input-buf)
           (with-current-buffer input-buf
             (let* ((pending pi-coding-agent--image-pending-send)
                    (current
                     (and pending
                          (= generation (plist-get pending :generation))
                          (= (plist-get pending :tick)
                             (buffer-chars-modified-tick))
                          (buffer-live-p chat-buf)
                          (= (or (plist-get pending :transition) 0)
                             (with-current-buffer chat-buf
                               pi-coding-agent--session-transition-generation)))))
               (when (and pending
                          (= generation (plist-get pending :generation)))
                 (setq pi-coding-agent--image-pending-send nil))
               (when current
                 (with-current-buffer chat-buf
                   (pi-coding-agent--apply-state-response chat-buf response))
                 (pcase (pi-coding-agent--image-status)
                   ('enabled
                    (pcase (plist-get pending :action)
                      ('steer (pi-coding-agent-queue-steering))
                      (_ (pi-coding-agent-send))))
                   ('unsupported
                    (message "Pi: Current model is text-only; select an image-capable model to send attachments"))
                   (_ (message "Pi: Image capability is still unavailable"))))))))))))

(defun pi-coding-agent--image-token-display (uuid record)
  "Return a display value for UUID and RECORD.
The textual token remains canonical, so history and undo retain identity even
on terminals or when Emacs cannot decode a particular image."
  (let* ((name (or (plist-get record :name) uuid))
         (status (if (pi-coding-agent--image-record-live-p record)
                     (pi-coding-agent--image-status)
                   'broken))
         (label (pcase status
                  ('broken (format "[image missing: %s]" name))
                  ('unsupported (format "[image unsupported: %s]" name))
                  ('unknown (format "[image pending: %s]" name))
                  (_ (format "[image: %s]" name)))))
    (if (and (eq status 'enabled) (display-images-p))
        (condition-case nil
            (create-image (plist-get record :path) nil nil
                          :max-width 96 :max-height 64 :ascent 'center)
          (error label))
      label)))

(defun pi-coding-agent--image-apply-token-properties (start end uuid)
  "Attach display identity properties to token from START to END for UUID."
  (let ((record (pi-coding-agent--image-record uuid)))
    (when record
      (remove-text-properties start end '(display nil face nil help-echo nil))
      (add-text-properties
       start end
       (list 'pi-coding-agent-image-uuid uuid
             'rear-nonsticky '(pi-coding-agent-image-uuid display)
             'front-sticky '(pi-coding-agent-image-uuid display)
             'face (pcase (if (pi-coding-agent--image-record-live-p record)
                              (pi-coding-agent--image-status)
                            'broken)
                     ('broken 'error)
                     ('unsupported 'warning)
                     ('unknown 'shadow)
                     (_ 'success))
             'help-echo (format "Image attachment: %s"
                                (or (plist-get record :name) uuid))
             'display (pi-coding-agent--image-token-display uuid record))))))

(defun pi-coding-agent--rehydrate-image-tokens (&optional start end)
  "Restore token properties between START and END from the current registry."
  (save-excursion
    (goto-char (or start (point-min)))
    (while (re-search-forward pi-coding-agent--image-token-regexp (or end (point-max)) t)
      (let ((uuid (match-string-no-properties 1)))
        (when (and (equal
                    (get-text-property (match-beginning 0)
                                       'pi-coding-agent-image-uuid)
                    uuid)
                   (pi-coding-agent--image-record uuid))
          (pi-coding-agent--image-apply-token-properties
           (match-beginning 0) (match-end 0) uuid))))))

(defun pi-coding-agent--insert-image-token (uuid &optional marker)
  "Insert UUID token at point or MARKER and render it."
  (when (and marker (marker-buffer marker))
    (goto-char marker))
  (let ((start (point)))
    (insert (pi-coding-agent--image-token uuid))
    (pi-coding-agent--image-apply-token-properties start (point) uuid)))

(defun pi-coding-agent--image-draft-envelope (&optional string)
  "Extract STRING or the visible draft into a text-plus-images envelope.
Only tokens carrying package identity properties and resolving in this input
buffer's registry are attachments; token-like ordinary text is retained."
  (let ((source (or string (buffer-string)))
        (position 0)
        (images nil)
        (text-parts nil))
    (while (string-match pi-coding-agent--image-token-regexp source position)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (uuid (match-string 1 source))
             (valid (and (get-text-property start 'pi-coding-agent-image-uuid source)
                         (pi-coding-agent--image-record uuid))))
        (push (substring source position start) text-parts)
        (if valid
            (push uuid images)
          (push (substring source start end) text-parts))
        (setq position end)))
    (push (substring source position) text-parts)
    (list :text (apply #'concat (nreverse text-parts))
          :images (nreverse images)
          :draft (copy-sequence source))))

(defun pi-coding-agent--image-envelope-p (value)
  "Return non-nil if VALUE is an internal text-plus-image envelope."
  (and (listp value) (plist-member value :text) (plist-member value :images)))

(defun pi-coding-agent--image-normalize-envelope (value)
  "Normalize legacy string VALUE to an internal message envelope."
  (if (pi-coding-agent--image-envelope-p value)
      value
    (list :text (or value "") :images nil :draft (or value ""))))

(defun pi-coding-agent--image-envelope-draft (envelope)
  "Return a propertized editable draft reconstructed from ENVELOPE."
  (or (and (stringp (plist-get envelope :draft))
           (copy-sequence (plist-get envelope :draft)))
      (let ((draft (copy-sequence (plist-get envelope :text))))
        (dolist (uuid (plist-get envelope :images))
          (setq draft
                (concat draft
                        (propertize
                         (pi-coding-agent--image-token uuid)
                         'pi-coding-agent-image-uuid uuid))))
        draft)))

(defun pi-coding-agent--image-validate-envelope (envelope)
  "Validate ENVELOPE against registry and configured message limits."
  (let ((images (plist-get envelope :images))
        (total 0))
    (when (> (length images) pi-coding-agent-image-message-max-count)
      (user-error "Pi: At most %d images are allowed per message"
                  pi-coding-agent-image-message-max-count))
    (dolist (uuid images)
      (let ((record (pi-coding-agent--image-record uuid)))
        (unless (pi-coding-agent--image-record-live-p record)
          (user-error "Pi: Image attachment %s is no longer readable" uuid))
        (setq total (+ total (or (plist-get record :base64-size) 0)))))
    (when (> total pi-coding-agent-image-message-max-base64-bytes)
      (user-error "Pi: Image payload is %d bytes; limit is %d"
                  total pi-coding-agent-image-message-max-base64-bytes))
    t))

(defun pi-coding-agent--serialize-image-envelope (envelope)
  "Return Pi RPC image objects for ENVELOPE's ordered attachment UUIDs."
  (pi-coding-agent--image-validate-envelope envelope)
  (mapcar
   (lambda (uuid)
     (let ((record (pi-coding-agent--image-record uuid)))
       (list :type "image"
             :data (base64-encode-string
                    (with-temp-buffer
                      (insert-file-contents-literally (plist-get record :path))
                      (buffer-string))
                    t)
             :mimeType (plist-get record :mime))))
   (plist-get envelope :images)))

(defun pi-coding-agent--prepare-image-envelope-for-rpc (envelope)
  "Return ENVELOPE with validated native RPC image objects attached."
  (if (null (plist-get envelope :images))
      envelope
    (let ((prepared (copy-sequence envelope)))
      (plist-put prepared :rpc-images
                 (pi-coding-agent--serialize-image-envelope envelope))
      prepared)))

(defun pi-coding-agent--image-uuid ()
  "Return a locally unique attachment identity."
  (substring (md5 (format "%s:%s:%s" (float-time) (random) (current-buffer))) 0 24))

(defun pi-coding-agent--image-mime-type (file)
  "Return supported MIME type for FILE according to ImageMagick content sniffing."
  (when (executable-find "magick")
    (with-temp-buffer
      (when (eq 0 (call-process "magick" nil (current-buffer) nil
                                "identify" "-format" "%m" file))
        (pcase (upcase (string-trim (buffer-string)))
          ("PNG" "image/png")
          ((or "JPG" "JPEG") "image/jpeg")
          ("GIF" "image/gif")
          ("WEBP" "image/webp"))))))

(defun pi-coding-agent--image-dimensions (file)
  "Return FILE dimensions as a cons cell, or nil when unavailable."
  (when (executable-find "magick")
    (with-temp-buffer
      (when (eq 0 (call-process "magick" nil (current-buffer) nil
                                "identify" "-format" "%w %h" file))
        (when-let* ((values (split-string (string-trim (buffer-string)) " " t))
                    ((= (length values) 2)))
          (cons (string-to-number (car values))
                (string-to-number (cadr values))))))))

(defun pi-coding-agent--normalize-image (source destination)
  "Normalize SOURCE with ImageMagick into DESTINATION and return metadata.
The first animation frame is selected, orientation and metadata are removed,
and the output is bounded before it enters the attachment registry."
  (unless (executable-find "magick")
    (user-error "Pi: ImageMagick `magick` is required for image attachments"))
  (unless (pi-coding-agent--image-mime-type source)
    (user-error "Pi: Supported image content is PNG, JPEG, GIF, or WebP"))
  (let ((resize (format "%dx%d>" pi-coding-agent-image-max-width
                        pi-coding-agent-image-max-height)))
    (unless (eq 0 (call-process
                   "magick" nil nil nil source "[0]" "-auto-orient" "-strip"
                   "-resize" resize "png24:" destination))
      (user-error "Pi: Image normalization failed")))
  (let* ((size (file-attribute-size (file-attributes destination)))
         (base64-size (* 4 (ceiling (/ (float size) 3)))))
    (when (> base64-size pi-coding-agent-image-max-base64-bytes)
      (delete-file destination)
      (user-error "Pi: Normalized image exceeds the %d byte base64 limit"
                  pi-coding-agent-image-max-base64-bytes))
    (let ((dimensions (pi-coding-agent--image-dimensions destination)))
      (list :mime "image/png"
            :width (car dimensions)
            :height (cdr dimensions)
            :size size
            :base64-size base64-size))))

(defvar-local pi-coding-agent--image-acquisition-processes nil
  "Live subprocesses owned by this input buffer's image acquisition jobs.")

(defun pi-coding-agent--image-job-cleanup (files marker)
  "Delete FILES and detach MARKER after a cancelled or failed acquisition."
  (dolist (file files)
    (when (file-exists-p file) (ignore-errors (delete-file file))))
  (when (markerp marker) (set-marker marker nil)))

(defun pi-coding-agent--start-image-normalization
    (source kind original-path marker &optional delete-source before-insert)
  "Asynchronously normalize SOURCE and insert its token at MARKER.
KIND and ORIGINAL-PATH become attachment metadata.  SOURCE is optionally
deleted when DELETE-SOURCE is non-nil.  BEFORE-INSERT, when non-nil, must
return non-nil before the token is inserted.  The job applies results only
while the originating input buffer and marker remain valid."
  (unless (executable-find "magick")
    (user-error "Pi: ImageMagick `magick` is required for image attachments"))
  (let* ((input-buf (current-buffer))
         (uuid (pi-coding-agent--image-uuid))
         (destination (expand-file-name (concat uuid ".png")
                                        (pi-coding-agent--image-directory)))
         (resize (format "%dx%d>" pi-coding-agent-image-max-width
                         pi-coding-agent-image-max-height))
         (stderr (generate-new-buffer " *pi-image-normalize-stderr*"))
         (process
          (make-process
           :name "pi-image-normalize"
           :buffer stderr
           :command (list "magick" (concat source "[0]")
                          "-format" "%m" "-write" "info:"
                          "-auto-orient" "-strip"
                          "-resize" resize (concat "png24:" destination))
           :noquery t
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let* ((source-format
                       (with-current-buffer (process-buffer proc)
                         (car (split-string (string-trim (buffer-string))
                                            "[[:space:]]+" t))))
                      (supported
                       (member (upcase (or source-format ""))
                               '("PNG" "JPG" "JPEG" "GIF" "WEBP")))
                      (ok (and supported
                              (= (process-exit-status proc) 0)
                              (file-readable-p destination)))
                     (message-text (with-current-buffer (process-buffer proc)
                                     (string-trim (buffer-string)))))
                 (kill-buffer (process-buffer proc))
                 (if (not ok)
                     (progn
                       (pi-coding-agent--image-job-cleanup
                        (delq nil (list destination (and delete-source source))) marker)
                       (if (and (= (process-exit-status proc) 0)
                                (not supported))
                           (message "Pi: Selected file is not a supported PNG, JPEG, GIF, or WebP image")
                         (message "Pi: Image normalization failed%s"
                                  (if (string-empty-p message-text)
                                      ""
                                    (concat ": " message-text)))))
                   (let ((metadata-buffer (generate-new-buffer " *pi-image-identify*")))
                     (make-process
                      :name "pi-image-identify"
                      :buffer metadata-buffer
                      :command (list "magick" "identify" "-format" "%w %h" destination)
                      :noquery t
                      :sentinel
                      (lambda (identify _identify-event)
                        (when (memq (process-status identify) '(exit signal))
                          (let* ((dimensions
                                  (with-current-buffer (process-buffer identify)
                                    (prog1 (split-string (string-trim (buffer-string)) " " t)
                                      (kill-buffer (current-buffer)))))
                                 (size (and (file-exists-p destination)
                                            (file-attribute-size (file-attributes destination))))
                                 (base64-size (and size (* 4 (ceiling (/ (float size) 3)))))
                                 (target-valid (and (buffer-live-p input-buf)
                                                    (marker-buffer marker)))
                                 (metadata-valid
                                  (and (= (process-exit-status identify) 0)
                                       (= (length dimensions) 2))))
                            (unwind-protect
                                (if (and metadata-valid
                                         target-valid
                                         size
                                         (<= base64-size pi-coding-agent-image-max-base64-bytes))
                                    (with-current-buffer input-buf
                                      (let (accepted)
                                        (when before-insert
                                          (undo-boundary))
                                        (atomic-change-group
                                          (when (or (null before-insert)
                                                    (funcall before-insert))
                                            (puthash uuid
                                                     (list :uuid uuid :path destination
                                                           :name (file-name-nondirectory
                                                                  (or original-path source))
                                                           :kind kind :original-path original-path
                                                           :owned t :mime "image/png"
                                                           :width (string-to-number (car dimensions))
                                                           :height (string-to-number (cadr dimensions))
                                                           :size size :base64-size base64-size)
                                                     (pi-coding-agent--image-registry))
                                            (pi-coding-agent--insert-image-token uuid marker)
                                            (setq accepted t)))
                                        (if accepted
                                            (message "Pi: Image attached")
                                          (pi-coding-agent--image-job-cleanup
                                           (list destination) marker))))
                                  (progn
                                    (pi-coding-agent--image-job-cleanup
                                     (list destination) marker)
                                    (when target-valid
                                      (cond
                                       ((not metadata-valid)
                                        (message "Pi: Image metadata inspection failed"))
                                       ((not size)
                                        (message "Pi: Normalized image output is missing"))
                                       ((> base64-size
                                           pi-coding-agent-image-max-base64-bytes)
                                        (message
                                         "Pi: Normalized image is %d base64 bytes; limit is %d"
                                         base64-size
                                         pi-coding-agent-image-max-base64-bytes))))))
                              (when delete-source
                                (when (file-exists-p source) (delete-file source)))
                              (when (markerp marker) (set-marker marker nil)))))))))))))))
    (push process pi-coding-agent--image-acquisition-processes)
    process))

(defun pi-coding-agent--register-normalized-image
    (path metadata kind &optional original-path)
  "Register owned PATH with METADATA and KIND and return its UUID.
ORIGINAL-PATH, when non-nil, records the non-owned source location."
  (let* ((uuid (pi-coding-agent--image-uuid))
         (record (append (list :uuid uuid :path path
                               :name (file-name-nondirectory
                                      (or original-path path))
                               :kind kind :original-path original-path
                               :owned t)
                         metadata)))
    (puthash uuid record (pi-coding-agent--image-registry))
    uuid))

(defun pi-coding-agent--import-image-file (file kind &optional original-path)
  "Copy local FILE acquired by KIND and return its attachment UUID.
ORIGINAL-PATH, when non-nil, is retained only as presentation metadata."
  (when (file-remote-p file)
    (user-error "Pi: Remote files cannot be attached as images"))
  (unless (and (file-regular-p file) (file-readable-p file))
    (user-error "Pi: Image must be a readable regular file"))
  (let* ((uuid (pi-coding-agent--image-uuid))
         (destination (expand-file-name (concat uuid ".png")
                                        (pi-coding-agent--image-directory))))
    (condition-case error-data
        (let ((metadata (funcall pi-coding-agent-image-normalize-function
                                 file destination)))
          ;; Register under the pre-generated UUID to keep destination and
          ;; token identity stable across a single undo/redo transaction.
          (puthash uuid
                   (append (list :uuid uuid :path destination
                                 :name (file-name-nondirectory
                                        (or original-path file))
                                 :kind kind :original-path original-path
                                 :owned t)
                           metadata)
                   (pi-coding-agent--image-registry))
          uuid)
      (error
       (when (file-exists-p destination) (delete-file destination))
       (signal (car error-data) (cdr error-data))))))

(defun pi-coding-agent--capture-clipboard-image (file)
  "Capture the macOS image clipboard into FILE using pngpaste."
  (unless (executable-find "pngpaste")
    (user-error "Pi: pngpaste is required for clipboard image attachment"))
  (let ((status (call-process "pngpaste" nil nil nil file)))
    (cond
     ((eq status 0) nil)
     ((eq status 1) 'no-image)
     (t (user-error "Pi: pngpaste failed to capture the clipboard image")))))

(defun pi-coding-agent-attach-clipboard-image (&optional yank-on-absence)
  "Capture a clipboard image and insert it as an attachment at point.
When YANK-ON-ABSENCE is non-nil, yank text at the original point if the
clipboard does not contain an image."
  (interactive)
  (if (not (eq pi-coding-agent-image-capture-function
               #'pi-coding-agent--capture-clipboard-image))
      ;; Preserve custom backend compatibility; default macOS capture is async.
      (let ((raw (make-temp-file "pi-coding-agent-clipboard-" nil ".png"))
            (marker (copy-marker (point) t)))
        (condition-case error-data
            (if (eq (funcall pi-coding-agent-image-capture-function raw) 'no-image)
                (progn
                  (when (file-exists-p raw) (delete-file raw))
                  (when (and yank-on-absence (marker-buffer marker))
                    (goto-char marker)
                    (call-interactively #'yank))
                  (set-marker marker nil)
                  'no-image)
              (pi-coding-agent--start-image-normalization
               raw 'clipboard nil marker t))
          (error
           (set-marker marker nil)
           (when (file-exists-p raw) (delete-file raw))
           (signal (car error-data) (cdr error-data)))))
    (unless (executable-find "pngpaste")
      (user-error "Pi: pngpaste is required for clipboard image attachment"))
    (let* ((input-buf (current-buffer))
           (raw (make-temp-file "pi-coding-agent-clipboard-" nil ".png"))
           (marker (copy-marker (point) t)))
      (make-process
       :name "pi-image-capture" :buffer nil :command (list "pngpaste" raw) :noquery t
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (pcase (process-exit-status proc)
             (0
              (when (buffer-live-p input-buf)
                (with-current-buffer input-buf
                  (pi-coding-agent--start-image-normalization
                   raw 'clipboard nil marker t))))
             (1
              (when (and yank-on-absence
                         (buffer-live-p input-buf)
                         (marker-buffer marker))
                (with-current-buffer input-buf
                  (goto-char marker)
                  (call-interactively #'yank)))
              (pi-coding-agent--image-job-cleanup (list raw) marker))
             (_
              (pi-coding-agent--image-job-cleanup (list raw) marker)
              (message "Pi: pngpaste failed to capture the clipboard image"))))))
      'pending)))

(defun pi-coding-agent-smart-yank ()
  "Attach a clipboard image, or delegate to normal `yank' when none exists."
  (interactive)
  (pi-coding-agent-attach-clipboard-image t))

(defun pi-coding-agent--image-uuid-at-point ()
  "Return attachment UUID at point, including immediately after a token."
  (or (get-text-property (point) 'pi-coding-agent-image-uuid)
      (and (> (point) (point-min))
           (get-text-property (1- (point))
                              'pi-coding-agent-image-uuid))))

(defun pi-coding-agent--remove-image-at-point ()
  "Remove the attachment token at point without deleting its backing data."
  (when-let* ((uuid (pi-coding-agent--image-uuid-at-point))
              (position (if (equal
                             (get-text-property
                              (point) 'pi-coding-agent-image-uuid)
                             uuid)
                            (point)
                          (1- (point))))
              (bounds (let ((start (previous-single-property-change
                                    (1+ position) 'pi-coding-agent-image-uuid
                                    nil (point-min)))
                            (end (next-single-property-change
                                  position 'pi-coding-agent-image-uuid
                                  nil (point-max))))
                        (cons start end))))
    (delete-region (car bounds) (cdr bounds))
    uuid))

(defun pi-coding-agent-remove-image-at-point ()
  "Remove the image attachment at point while retaining undo backing data."
  (interactive)
  (unless (pi-coding-agent--remove-image-at-point)
    (user-error "Pi: No image attachment at point")))

(defun pi-coding-agent-preview-image-at-point ()
  "Open the attachment at point in the temporary Agent Workspace Workbench."
  (interactive)
  (let* ((uuid (pi-coding-agent--image-uuid-at-point))
         (record (and uuid (pi-coding-agent--image-record uuid))))
    (unless (pi-coding-agent--image-record-live-p record)
      (user-error "Pi: No readable image attachment at point"))
    (unless (require 'cabins-agent-workspace nil t)
      (user-error "Pi: Agent Workspace is required for image preview"))
    (let ((origin (selected-window))
          (buffer (find-file-noselect (plist-get record :path))))
      (with-current-buffer buffer
        (image-mode))
      (cabins-agent-workspace-open-temporary-workbench buffer)
      (when (window-live-p origin)
        (select-window origin)))))

(defun pi-coding-agent-input-newline-or-preview ()
  "Preview an attachment at point, or retain the normal input newline action."
  (interactive)
  (if (pi-coding-agent--image-uuid-at-point)
      (pi-coding-agent-preview-image-at-point)
    (newline-and-indent)))

(defun pi-coding-agent--cleanup-image-attachments ()
  "Delete only temporary files owned by the current input client."
  (dolist (process pi-coding-agent--image-acquisition-processes)
    (when (process-live-p process)
      (set-process-sentinel process nil)
      (delete-process process)))
  (setq pi-coding-agent--image-acquisition-processes nil)
  (when (hash-table-p pi-coding-agent--image-attachments)
    (maphash
     (lambda (_uuid record)
       (when (and (plist-get record :owned)
                  (file-exists-p (plist-get record :path)))
         (ignore-errors (delete-file (plist-get record :path)))))
     pi-coding-agent--image-attachments)
    (clrhash pi-coding-agent--image-attachments))
  (when (and pi-coding-agent--image-attachment-directory
             (file-directory-p pi-coding-agent--image-attachment-directory))
    (ignore-errors (delete-directory pi-coding-agent--image-attachment-directory t)))
  (setq pi-coding-agent--image-attachment-directory nil))

;;;; Input History (comint/eshell style)

(defvar pi-coding-agent--input-ring-size 100
  "Size of the input history ring.")

(defvar-local pi-coding-agent--input-ring nil
  "Ring holding input history for this session.")

(defvar-local pi-coding-agent--input-ring-index nil
  "Current position in input ring, or nil if not navigating history.")

(defvar-local pi-coding-agent--input-saved nil
  "Saved input before starting history navigation.")

(defvar-local pi-coding-agent--history-isearch-active nil
  "Non-nil when history isearch is active.")

(defvar-local pi-coding-agent--history-isearch-saved-input nil
  "Saved input before starting history isearch.")

(defvar-local pi-coding-agent--history-isearch-index nil
  "Current history index during isearch.")

(defun pi-coding-agent--input-ring ()
  "Return the input ring, creating if necessary."
  (unless pi-coding-agent--input-ring
    (setq pi-coding-agent--input-ring (make-ring pi-coding-agent--input-ring-size)))
  pi-coding-agent--input-ring)

(defun pi-coding-agent--history-add (input)
  "Add INPUT to history ring if non-empty and different from last."
  (let ((ring (pi-coding-agent--input-ring))
        (trimmed (and input (string-trim (copy-sequence input)))))
    (when (and trimmed
               (not (string-empty-p trimmed))
               (or (ring-empty-p ring)
                   (not (string= trimmed (ring-ref ring 0)))))
      (ring-insert ring trimmed))))

(defun pi-coding-agent-previous-input ()
  "Cycle backwards through input history.
Saves current input before first navigation."
  (interactive)
  (let ((ring (pi-coding-agent--input-ring)))
    (when (ring-empty-p ring)
      (user-error "No history"))
    (unless pi-coding-agent--input-ring-index
      (setq pi-coding-agent--input-saved (buffer-string)))
    (let ((new-index (if pi-coding-agent--input-ring-index
                         (1+ pi-coding-agent--input-ring-index)
                       0)))
      (if (>= new-index (ring-length ring))
          (user-error "Beginning of history")
        (setq pi-coding-agent--input-ring-index new-index)
        (delete-region (point-min) (point-max))
        (insert (ring-ref ring new-index))
        (pi-coding-agent--rehydrate-image-tokens)))))

(defun pi-coding-agent-next-input ()
  "Cycle forwards through input history.
Restores saved input when moving past newest entry."
  (interactive)
  (unless pi-coding-agent--input-ring-index
    (user-error "End of history"))
  (let ((new-index (1- pi-coding-agent--input-ring-index)))
    (delete-region (point-min) (point-max))
    (if (< new-index 0)
        (progn
          (setq pi-coding-agent--input-ring-index nil)
          (when pi-coding-agent--input-saved
            (insert pi-coding-agent--input-saved)
            (pi-coding-agent--rehydrate-image-tokens)))
      (setq pi-coding-agent--input-ring-index new-index)
      (insert (ring-ref (pi-coding-agent--input-ring) new-index))
      (pi-coding-agent--rehydrate-image-tokens))))

;;;; History Isearch

(defun pi-coding-agent-history-isearch-backward ()
  "Search input history backward using isearch.
Incrementally search through history with matches appearing
directly in the input buffer, like readline."
  (interactive)
  (let ((ring (pi-coding-agent--input-ring)))
    (when (ring-empty-p ring)
      (user-error "No history"))
    (setq pi-coding-agent--history-isearch-active t
          pi-coding-agent--history-isearch-saved-input (buffer-string)
          pi-coding-agent--history-isearch-index nil)
    (isearch-backward nil t)))

(defun pi-coding-agent--history-isearch-setup ()
  "Configure isearch for history searching."
  (when pi-coding-agent--history-isearch-active
    (setq isearch-message-prefix-add "history ")
    (setq-local isearch-search-fun-function
                #'pi-coding-agent--history-isearch-search-fun)
    (setq-local isearch-wrap-function
                #'pi-coding-agent--history-isearch-wrap)
    (setq-local isearch-push-state-function
                #'pi-coding-agent--history-isearch-push-state)
    (setq-local isearch-lazy-count nil)
    (add-hook 'isearch-mode-end-hook
              #'pi-coding-agent--history-isearch-end nil t)))

(defun pi-coding-agent--history-isearch-end ()
  "Clean up after history isearch ends.
Restore original input if isearch was quit, keep history item if accepted."
  (setq isearch-message-prefix-add nil)
  (setq-local isearch-search-fun-function #'isearch-search-fun-default)
  (setq-local isearch-wrap-function nil)
  (setq-local isearch-push-state-function nil)
  (kill-local-variable 'isearch-lazy-count)
  (remove-hook 'isearch-mode-end-hook #'pi-coding-agent--history-isearch-end t)
  (when isearch-mode-end-hook-quit
    (delete-region (point-min) (point-max))
    (insert (or pi-coding-agent--history-isearch-saved-input ""))
    (pi-coding-agent--rehydrate-image-tokens))
  (unless isearch-suspended
    (setq pi-coding-agent--history-isearch-active nil
          pi-coding-agent--history-isearch-saved-input nil
          pi-coding-agent--history-isearch-index nil)))

(defun pi-coding-agent--history-isearch-goto (index)
  "Load history item at INDEX into the buffer.
If INDEX is nil, restore saved input (current line content before search)."
  (setq pi-coding-agent--history-isearch-index index)
  (delete-region (point-min) (point-max))
  (if (and index (not (ring-empty-p (pi-coding-agent--input-ring))))
      (insert (ring-ref (pi-coding-agent--input-ring) index))
    (when (and pi-coding-agent--history-isearch-saved-input
               (> (length pi-coding-agent--history-isearch-saved-input) 0))
      (insert pi-coding-agent--history-isearch-saved-input)))
  (pi-coding-agent--rehydrate-image-tokens))

(defun pi-coding-agent--history-isearch-search-fun ()
  "Return search function for history isearch.
First searches current buffer text, then cycles through history."
  (lambda (string bound noerror)
    (let ((search-fun (isearch-search-fun-default))
          (ring (pi-coding-agent--input-ring))
          found)
      (or
       (funcall search-fun string bound noerror)
       (unless bound
         (condition-case nil
             (progn
               (while (not found)
                 (cond
                  (isearch-forward
                   (when (null pi-coding-agent--history-isearch-index)
                     (error "End of history; no next item"))
                   (let ((new-idx (1- pi-coding-agent--history-isearch-index)))
                     (if (< new-idx 0)
                         (pi-coding-agent--history-isearch-goto nil)
                       (pi-coding-agent--history-isearch-goto new-idx)))
                   (goto-char (point-min)))
                  (t
                   (let* ((cur-idx (or pi-coding-agent--history-isearch-index -1))
                          (new-idx (1+ cur-idx)))
                     (when (>= new-idx (ring-length ring))
                       (error "Beginning of history; no preceding item"))
                     (pi-coding-agent--history-isearch-goto new-idx))
                   (goto-char (point-max))))
                 (setq isearch-barrier (point)
                       isearch-opoint (point))
                 (setq found (funcall search-fun string nil noerror)))
               (point))
           (error nil)))))))

(defun pi-coding-agent--history-isearch-wrap ()
  "Wrap history isearch to beginning/end of history.
For forward search: go to oldest history item.
For backward search: go to current input (nil index)."
  (pi-coding-agent--history-isearch-goto
   (if isearch-forward
       (1- (ring-length (pi-coding-agent--input-ring)))
     nil))
  (goto-char (if isearch-forward (point-min) (point-max))))

(defun pi-coding-agent--history-isearch-push-state ()
  "Save history index for isearch state restoration."
  (let ((index pi-coding-agent--history-isearch-index))
    (lambda (_cmd)
      (pi-coding-agent--history-isearch-goto index))))

;;;; Input Mode

(defun pi-coding-agent--input-kill-buffer-query ()
  "Ask before killing input when its linked chat owns a live process."
  (pi-coding-agent--session-kill-buffer-query))

(define-derived-mode pi-coding-agent-input-mode text-mode "Pi-Input"
  "Major mode for composing pi prompts.
Uses tree-sitter markdown highlighting by default while preserving raw
markup visibility, mode identity, and keybindings.  Set
`pi-coding-agent-input-markdown-highlighting' to nil for plain text."
  :group 'pi-coding-agent
  (when pi-coding-agent-input-markdown-highlighting
    (md-ts-mode)
    (setq major-mode 'pi-coding-agent-input-mode)
    (setq mode-name "Pi-Input")
    (use-local-map pi-coding-agent-input-mode-map)
    ;; Users see exactly what they type — never hide markup in input.
    (setq-local md-ts-hide-markup nil)
    (md-ts--set-hide-markup nil))
  (setq-local header-line-format '(:eval (pi-coding-agent--header-line-string)))
  ;; Reset inherited completions (text-mode adds ispell, etc.) — our
  ;; input buffer should only offer slash commands, file refs, and paths.
  (setq-local completion-at-point-functions nil)
  (add-hook 'completion-at-point-functions #'pi-coding-agent--command-capf nil t)
  (add-hook 'completion-at-point-functions #'pi-coding-agent--file-reference-capf nil t)
  (add-hook 'completion-at-point-functions #'pi-coding-agent--path-capf nil t)
  (add-hook 'post-self-insert-hook #'pi-coding-agent--maybe-complete-at nil t)
  (add-hook 'after-change-functions
            (lambda (&optional begin end _old)
              (pi-coding-agent--rehydrate-image-tokens begin end))
            nil t)
  (add-hook 'pi-coding-agent-input-state-change-hook
            #'pi-coding-agent--rehydrate-image-tokens nil t)
  (add-hook 'isearch-mode-hook #'pi-coding-agent--history-isearch-setup nil t)
  (add-hook 'kill-buffer-query-functions
            #'pi-coding-agent--input-kill-buffer-query nil t)
  (add-hook 'kill-buffer-hook #'pi-coding-agent--cleanup-input-on-kill nil t)
  (add-hook 'kill-buffer-hook #'pi-coding-agent--cleanup-image-attachments nil t))

;;;; Input-Buffer Chat Navigation

(defun pi-coding-agent--call-in-visible-chat-window (fn)
  "Call FN in the visible linked chat window, preserving input focus."
  (let* ((chat-buf (pi-coding-agent--get-chat-buffer))
         (win (and (buffer-live-p chat-buf)
                   (get-buffer-window chat-buf))))
    (if (window-live-p win)
        (save-selected-window
          (select-window win)
          (funcall fn))
      (user-error "No chat window visible"))))

(defun pi-coding-agent-input-next-message ()
  "Move chat to the next user message, keeping focus in input."
  (interactive)
  (pi-coding-agent--call-in-visible-chat-window
   #'pi-coding-agent-next-message))

(defun pi-coding-agent-input-previous-message ()
  "Move chat to the previous user message, keeping focus in input."
  (interactive)
  (pi-coding-agent--call-in-visible-chat-window
   #'pi-coding-agent-previous-message))

;;;; Sending Prompts

(defun pi-coding-agent--accept-input-text (text)
  "Accept TEXT or a propertized draft from input buffer state.
Adds the original string to history, resets history navigation, and clears
input.  The historical name remains for compatibility with callers."
  (pi-coding-agent--history-add text)
  (setq pi-coding-agent--input-ring-index nil
        pi-coding-agent--input-saved nil)
  (erase-buffer))

(defun pi-coding-agent--queue-followup-text (chat-buf message &optional draft)
  "Accept DRAFT and enqueue complete MESSAGE in CHAT-BUF.
Legacy string callers may omit DRAFT."
  (pi-coding-agent--accept-input-text (or draft message))
  (with-current-buffer chat-buf
    (pi-coding-agent--push-followup message)))

(defun pi-coding-agent-send ()
  "Send the current input buffer contents to pi.
Clears the input buffer after sending.  Does nothing if buffer is empty.
If pi is busy (sending, streaming, or compacting), queues a local follow-up.
The /compact command is handled locally; other slash commands sent to pi."
  (interactive)
  (let* ((draft (buffer-string))
         (envelope (pi-coding-agent--image-draft-envelope draft))
         (text (string-trim (plist-get envelope :text)))
         (images (plist-get envelope :images))
         (_ (plist-put envelope :text text))
         (message (if images envelope text))
         (chat-buf (pi-coding-agent--get-chat-buffer))
         (chat-live-p (buffer-live-p chat-buf))
         (transitioning (and chat-live-p
                             (pi-coding-agent--session-transition-active-p
                              chat-buf)))
         (busy (and chat-live-p
                    (pi-coding-agent--session-busy-p chat-buf))))
    (cond
     ((and (string-empty-p text) (null images)) nil)
     ((not chat-live-p)
      (message "Pi: No chat session available"))
     (transitioning
      (message "Pi: Cannot send while session is switching"))
     ((and images (pi-coding-agent--builtin-command-text-p text))
      (message "Pi: Local /%s commands cannot include images"
               (pi-coding-agent--builtin-command-name text)))
     ((and images (eq (pi-coding-agent--image-status) 'unsupported))
      (message "Pi: Current model is text-only; select an image-capable model to send attachments"))
     ((and images (eq (pi-coding-agent--image-status) 'unknown))
      (pi-coding-agent--resolve-image-capability-and-resend chat-buf))
     ((and busy (pi-coding-agent--builtin-command-text-p text))
      (message "Pi: Cannot queue /%s while Pi is busy"
               (pi-coding-agent--builtin-command-name text)))
     (busy
      (pi-coding-agent--image-validate-envelope envelope)
      (pi-coding-agent--queue-followup-text
       chat-buf message (if images draft text))
      (message "Pi: Message queued (will send when Pi is ready)"))
     (t
      (pi-coding-agent--image-validate-envelope envelope)
      (when images
        (setq message (pi-coding-agent--prepare-image-envelope-for-rpc envelope)))
      (pi-coding-agent--accept-input-text (if images draft text))
      (with-current-buffer chat-buf
        (pi-coding-agent--prepare-and-send message))))))

(defun pi-coding-agent-abort ()
  "Abort the current pi operation.
Works while sending, streaming, or compacting."
  (interactive)
  (when-let* ((chat-buf (pi-coding-agent--get-chat-buffer)))
    (let ((status (buffer-local-value 'pi-coding-agent--status chat-buf)))
      (when (memq status '(sending streaming compacting))
        (when (eq status 'streaming)
          (with-current-buffer chat-buf
            (pi-coding-agent--set-aborted t)))
        (with-current-buffer chat-buf
          (unless (pi-coding-agent--restore-followup-queue-to-input)
            (message "Pi: Follow-up recovery is pending because the input buffer is unavailable")))
        (when-let* ((proc (pi-coding-agent--get-process)))
          (pi-coding-agent--rpc-async proc
                         (list :type "abort")
                         (lambda (_response)
                           (run-with-timer 2 nil (lambda () (message nil)))
                           (message "Pi: Aborted"))))))))

(defun pi-coding-agent-quit ()
  "Close the current pi session.
Kills both chat and input buffers, terminates the process,
and removes the input window (merging its space with adjacent windows).

If a process is running, asks for confirmation first unless
`pi-coding-agent-quit-without-confirmation' is non-nil.  If the user
cancels, the session remains intact."
  (interactive)
  (let* ((chat-buf (pi-coding-agent--get-chat-buffer))
         (input-buf (pi-coding-agent--get-input-buffer))
         (proc (when (buffer-live-p chat-buf)
                 (buffer-local-value 'pi-coding-agent--process chat-buf)))
         (proc-live (and proc (process-live-p proc)))
         (input-windows nil))
    (when (and (pi-coding-agent--process-kill-confirmation-required-p proc)
               (not (yes-or-no-p "Pi session has a running process; quit anyway? ")))
      (user-error "Quit cancelled"))
    ;; Suppress Emacs and pi buffer-kill prompts after explicit confirmation.
    (when proc-live
      (pi-coding-agent--skip-process-kill-confirmation proc)
      (set-process-query-on-exit-flag proc nil))
    (when (buffer-live-p input-buf)
      (setq input-windows (get-buffer-window-list input-buf nil t)))
    ;; Kill chat first — its cleanup hook cascades to input buffer
    (when (buffer-live-p chat-buf)
      (kill-buffer chat-buf))
    (when (buffer-live-p input-buf)
      (kill-buffer input-buf))
    (dolist (win input-windows)
      (when (window-live-p win)
        (ignore-errors (delete-window win))))))

;;;; Slash Command Completion

(defun pi-coding-agent--command-capf ()
  "Completion-at-point function for /commands in input buffer.
Returns completion data when point is after / at start of buffer.
Includes both built-in commands and commands from pi's `get_commands' RPC."
  (when (and (eq (char-after (point-min)) ?/)
             (> (point) (point-min)))
    (let* ((start (1+ (point-min)))
           (end (save-excursion
                  (goto-char start)
                  (skip-chars-forward "^ \t\n")
                  (point)))
           (builtin-names (mapcar #'car pi-coding-agent--builtin-commands))
           (rpc-names (mapcar (lambda (cmd) (plist-get cmd :name))
                              pi-coding-agent--commands))
           (commands (delete-dups (append builtin-names rpc-names))))
      (when (<= start (point) end)
        (list start end commands :exclusive 'no)))))

;;;; Editor Features: File Reference (@)

(defun pi-coding-agent--at-trigger-p ()
  "Return non-nil if @ at point should trigger file completion.
Returns nil when @ follows an alphanumeric character (like in emails).
Assumes point is right after the @."
  (or (< (point) 3)  ; @ at buffer start or position 2 (no char before @)
      (save-excursion
        (backward-char 2)  ; Move to char before @
        (looking-at-p "[^[:alnum:]]"))))

(defun pi-coding-agent--maybe-complete-at ()
  "Trigger completion after @ if at word boundary.
Called from `post-self-insert-hook'.
Does not trigger when @ follows alphanumeric (e.g., in email addresses)."
  (when (and (eq last-command-event ?@)
             (pi-coding-agent--at-trigger-p))
    (run-at-time 0 nil #'pi-coding-agent--complete-file-reference)))

(defun pi-coding-agent--complete-file-reference ()
  "Complete file reference after @."
  (let* ((files (pi-coding-agent--get-project-files))
         (choice (completing-read "File: " files nil nil)))
    (when (and choice (not (string-empty-p choice)))
      (let ((start (point)))
        (insert choice)
        (pi-coding-agent--accept-file-reference-completion
         (1- start) (point) choice)))))

(defun pi-coding-agent--accepted-image-file (candidate)
  "Return CANDIDATE's local absolute file name when it can be inspected."
  (let ((file (expand-file-name candidate (pi-coding-agent--session-directory))))
    (cond
     ((file-remote-p file)
      (message "Pi: Remote files cannot be attached as images")
      nil)
     ((not (file-exists-p file))
      (message "Pi: Selected attachment file does not exist")
      nil)
     ((not (file-regular-p file))
      (message "Pi: Image attachment must be a regular file")
      nil)
     ((not (file-readable-p file))
      (message "Pi: Image attachment is not readable")
      nil)
     (t file))))

(defun pi-coding-agent--accept-file-reference-completion (start end candidate)
  "Convert accepted local image CANDIDATE between START and END to a token.
Non-image completion candidates remain ordinary `@file' text."
  (let ((candidate
         (or (and (> (length candidate) 0)
                  (get-text-property 0 'helm-realvalue candidate))
             (string-trim-right (substring-no-properties candidate)))))
    (when-let* ((file (pi-coding-agent--accepted-image-file candidate)))
      (let* ((start-marker (copy-marker start nil))
             (end-marker (copy-marker end nil))
             (reference (buffer-substring-no-properties start end)))
        (pi-coding-agent--start-image-normalization
         file 'file file start-marker nil
         (lambda ()
           (when (and (marker-buffer start-marker)
                      (marker-buffer end-marker)
                      (equal (buffer-substring-no-properties
                              start-marker end-marker)
                             reference))
             (delete-region start-marker end-marker)
             t)))))))

(defun pi-coding-agent--file-reference-completion-exit (candidate status)
  "Import CAPF CANDIDATE when STATUS reports completed acceptance."
  (when (eq status 'finished)
    (let ((end (point))
          (start (save-excursion
                   (search-backward "@" (line-beginning-position) t)
                   (point))))
      (when (and start candidate)
        (pi-coding-agent--accept-file-reference-completion start end candidate)))))

(defvar-local pi-coding-agent--project-files-cache nil
  "Cached list of project files for @ completion.")

(defvar-local pi-coding-agent--project-files-cache-time nil
  "Time when project files cache was last updated.")

(defconst pi-coding-agent--project-files-cache-ttl 30
  "Seconds before project files cache expires.")

(defconst pi-coding-agent--file-exclude-patterns
  '(".git" "node_modules" ".elpa" "target" "build" "__pycache__" ".venv" "dist")
  "Directory names to exclude when listing files with find.")

(defun pi-coding-agent--get-project-files ()
  "Get list of project files, respecting .gitignore.
Uses cache if available and not expired."
  (let ((now (float-time)))
    (when (or (null pi-coding-agent--project-files-cache)
              (null pi-coding-agent--project-files-cache-time)
              (> (- now pi-coding-agent--project-files-cache-time)
                 pi-coding-agent--project-files-cache-ttl))
      (setq pi-coding-agent--project-files-cache
            (pi-coding-agent--list-project-files))
      (setq pi-coding-agent--project-files-cache-time now))
    pi-coding-agent--project-files-cache))

(defun pi-coding-agent--list-project-files ()
  "List project files using git ls-files or find.
Respects .gitignore when in a git repository."
  (let* ((dir (pi-coding-agent--session-directory))
         (default-directory dir))
    (condition-case nil
        (let ((output (shell-command-to-string
                       "git ls-files --cached --others --exclude-standard 2>/dev/null")))
          (if (string-empty-p output)
              (pi-coding-agent--list-files-with-find dir)
            (split-string output "\n" t)))
      (error (pi-coding-agent--list-files-with-find dir)))))

(defun pi-coding-agent--list-files-with-find (dir)
  "List files in DIR using find.
Excludes directories listed in `pi-coding-agent--file-exclude-patterns'."
  (let* ((default-directory dir)
         (prune-expr (mapconcat (lambda (p) (format "-name '%s'" p))
                                pi-coding-agent--file-exclude-patterns
                                " -o "))
         (cmd (format "find . \\( %s \\) -prune -o -type f -print 2>/dev/null | sed 's|^\\./||'"
                      prune-expr)))
    (split-string (shell-command-to-string cmd) "\n" t)))

(defun pi-coding-agent--file-reference-capf ()
  "Completion-at-point function for @file references.
Triggers when @ is typed, provides completion of project files."
  (when-let* ((at-pos (save-excursion
                        (when (search-backward "@" (line-beginning-position) t)
                          (point)))))
    (let* ((start (1+ at-pos))
           (end (point))
           (files (pi-coding-agent--get-project-files)))
      (when files
        (list start end files
              :exclusive 'no
              :annotation-function (lambda (_) " (file)")
              :exit-function #'pi-coding-agent--file-reference-completion-exit
              :company-kind (lambda (_) 'file))))))

;;;; Editor Features: Path Completion

(defun pi-coding-agent--path-prefix-p (path)
  "Check if PATH has a completable prefix (./, ../, ~/, or /)."
  (or (string-prefix-p "./" path)
      (string-prefix-p "../" path)
      (string-prefix-p "~/" path)
      (string-prefix-p "/" path)))

(defun pi-coding-agent--path-completions (path)
  "Return file completion candidates for PATH, or nil if directory invalid."
  (condition-case nil
      (let* ((dir (pi-coding-agent--route-preserving-file-name-directory path))
             (base (file-name-nondirectory path))
             (session-dir (pi-coding-agent--session-directory))
             (expanded-dir (if dir
                               (pi-coding-agent--route-preserving-file-name-as-directory
                                (pi-coding-agent--emacs-path dir session-dir))
                             session-dir)))
        (when (file-directory-p expanded-dir)
          (mapcar (lambda (f) (concat (or dir "") f))
                  (cl-remove-if (lambda (f) (member f '("." ".." "./" "../")))
                                (file-name-all-completions base expanded-dir)))))
    (error nil)))

(defun pi-coding-agent--path-capf ()
  "Completion-at-point function for file paths.
Completes paths starting with ./, ../, ~/, or /.
Skips / at buffer start to allow slash command completion."
  (when-let* ((bounds (bounds-of-thing-at-point 'filename))
              (start (car bounds))
              (end (cdr bounds))
              (path (buffer-substring-no-properties start end))
              ((pi-coding-agent--path-prefix-p path))
              ((not (and (string-prefix-p "/" path)
                         (= start (point-min)))))
              (candidates (pi-coding-agent--path-completions path)))
    (list start end candidates
          :exclusive 'no
          :annotation-function
          (lambda (c)
            (if (string-suffix-p "/" c) " (dir)" " (file)")))))

;;;; Editor Features: Message Queuing

(defun pi-coding-agent--send-steer-message (text)
  "Send TEXT as a steering message via RPC.
Returns t if message was sent, nil if process unavailable.
Shows error message if RPC fails."
  (let* ((envelope (pi-coding-agent--image-normalize-envelope text))
         (message (plist-get envelope :text))
         (images (plist-get envelope :images))
         (rpc-images (plist-get envelope :rpc-images))
         (proc (pi-coding-agent--get-process)))
    (if (and proc (process-live-p proc))
        (progn
          (pi-coding-agent--rpc-async proc
                                      (append (list :type "steer" :message message)
                                              (when images
                                                (list :images (vconcat rpc-images))))
                                      (lambda (response)
                                        (unless (eq (plist-get response :success) t)
                                          (message "Pi: Steering failed: %s"
                                                   (or (plist-get response :error) "unknown error")))))
          t)
      (message "Pi: Cannot send steering - process unavailable")
      nil)))

(defun pi-coding-agent-queue-steering ()
  "Send current input as a steering message.
When pi is sending or streaming, steering interrupts remaining tools.
Unlike normal sends, steering is NOT displayed locally - pi will echo
it back via message_start at the correct position (after current
assistant output completes).

When compaction is in progress, steering text is queued as a local
follow-up.  It is sent after non-retry compaction, or after Pi's
automatic overflow retry turn finishes."
  (interactive)
  (let* ((draft (buffer-string))
         (envelope (pi-coding-agent--image-draft-envelope draft))
         (text (string-trim (plist-get envelope :text)))
         (images (plist-get envelope :images))
         (_ (plist-put envelope :text text))
         (message (if images envelope text)))
    (unless (and (string-empty-p text) (null images))
      (let ((chat-buf (pi-coding-agent--get-chat-buffer)))
        (when chat-buf
          (let ((status (buffer-local-value 'pi-coding-agent--status chat-buf)))
            (cond
             ((pi-coding-agent--session-transition-active-p chat-buf)
              (message "Pi: Cannot send steering while session is switching"))
             ((and images (pi-coding-agent--builtin-command-text-p text))
              (message "Pi: Local /%s commands cannot include images"
                       (pi-coding-agent--builtin-command-name text)))
             ((and images (eq (pi-coding-agent--image-status) 'unsupported))
              (message "Pi: Current model is text-only; select an image-capable model to send attachments"))
             ((and images (eq (pi-coding-agent--image-status) 'unknown))
              (pi-coding-agent--resolve-image-capability-and-resend chat-buf 'steer))
             ((and (eq status 'idle)
                   (not (pi-coding-agent--session-busy-p chat-buf)))
              (message "Pi: Nothing to interrupt - use C-c C-c to send"))
             ((or (eq status 'compacting)
                  (and (eq status 'idle)
                       (pi-coding-agent--session-busy-p chat-buf)))
              (pi-coding-agent--image-validate-envelope envelope)
              (pi-coding-agent--queue-followup-text
               chat-buf message (if images draft text))
              (message "Pi: Steering queued (will send when Pi is ready)"))
             ((memq status '(sending streaming))
              (pi-coding-agent--image-validate-envelope envelope)
              (when images
                (setq message
                      (pi-coding-agent--prepare-image-envelope-for-rpc envelope)))
              (when (pi-coding-agent--send-steer-message message)
                (pi-coding-agent--accept-input-text (if images draft text))
                (message "Pi: Steering message sent")))
             (t
              (message "Pi: Cannot steer while session status is %s" status)))))))))

(defun pi-coding-agent-queue-followup ()
  "Queue current input as a follow-up message.
Obsolete: Use `pi-coding-agent-send' (C-c C-c) instead, which now
automatically queues as follow-up when the agent is busy."
  (interactive)
  (pi-coding-agent-send))
(make-obsolete 'pi-coding-agent-queue-followup 'pi-coding-agent-send "1.3.0")

(provide 'pi-coding-agent-input)
;;; pi-coding-agent-input.el ends here
