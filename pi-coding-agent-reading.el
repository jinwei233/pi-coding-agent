;;; pi-coding-agent-reading.el --- Turn reading projections -*- lexical-binding: t; -*-

;;; Commentary:
;; Companion indexes for render.el.  Canonical text is retained once per
;; thought, never copied onto each character of the collapsed projection.

;;; Code:
(require 'pi-coding-agent-ui)
(require 'cl-lib)
(require 'button)
(require 'text-property-search)

(defvar pi-coding-agent--current-activity-turn)
(defvar pi-coding-agent--streaming-marker)
(declare-function pi-coding-agent--activity-current-turn "pi-coding-agent-render" ())
(declare-function pi-coding-agent--single-line-preview "pi-coding-agent-render" (text))
(declare-function pi-coding-agent--activity-row-text "pi-coding-agent-render" (cue type content &optional cue-face))
(declare-function pi-coding-agent--first-nonempty-line "pi-coding-agent-render" (text &optional limit))
(declare-function pi-coding-agent--thinking-blockquote-text "pi-coding-agent-render" (text))
(declare-function pi-coding-agent--thinking-normalize-text "pi-coding-agent-render" (text))
(declare-function pi-coding-agent--ensure-blank-line-before-block "pi-coding-agent-render" ())
(declare-function pi-coding-agent--activity-record-for-turn "pi-coding-agent-render" (turn-id &optional create))
(declare-function pi-coding-agent--activity-record-start-marker "pi-coding-agent-render" (record))
(declare-function pi-coding-agent--activity-record-tools "pi-coding-agent-render" (record))
(declare-function pi-coding-agent--activity-render-record "pi-coding-agent-render" (record))
(declare-function pi-coding-agent--activity-toggle-tool-detail "pi-coding-agent-render" (record tool))
(declare-function pi-coding-agent--capture-window-rewrite-states "pi-coding-agent-render" ())
(declare-function pi-coding-agent--restore-window-rewrite-states "pi-coding-agent-render" (buffer states &optional adjust))

(defcustom pi-coding-agent-thinking-activity-display 'blocks
  "Display completed thinking as individual `blocks' or turn `grouped'."
  :type '(choice (const blocks) (const grouped))
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-reading-hints t
  "Whether Output windows show their independent reading status."
  :type 'boolean :group 'pi-coding-agent)

(cl-defstruct (pi-coding-agent--reading-turn
               (:constructor pi-coding-agent--make-reading-turn))
  id boundary answer message-answer state separator
  thoughts thought-tail start end expanded selected)

(defvar-local pi-coding-agent--reading-turns nil)
(defvar-local pi-coding-agent--reading-current nil)
(defvar-local pi-coding-agent--reading-title nil)
(defvar-local pi-coding-agent--reading-welcome-hidden nil)
(defvar-local pi-coding-agent--reading-generation 0)
(defvar-local pi-coding-agent--reading-chunks nil)
(defvar-local pi-coding-agent--reading-live-thought nil)
(defvar pi-coding-agent--reading-history nil)

(defun pi-coding-agent--reading-grouped-p ()
  "Return whether this chat uses grouped, folded thinking."
  (and (eq pi-coding-agent-thinking-activity-display 'grouped)
       (not (eq (pi-coding-agent--thinking-display-mode) 'visible))))

(defun pi-coding-agent--reading-clear ()
  "Release projection state before rebuilding a transcript."
  (dolist (turn pi-coding-agent--reading-turns)
    (when (overlayp (pi-coding-agent--reading-turn-separator turn))
      (delete-overlay (pi-coding-agent--reading-turn-separator turn))))
  (setq pi-coding-agent--reading-turns nil
        pi-coding-agent--reading-current nil
        pi-coding-agent--reading-title nil
        pi-coding-agent--reading-welcome-hidden nil
        pi-coding-agent--reading-chunks nil
        pi-coding-agent--reading-live-thought nil))

(defun pi-coding-agent--reading-first-request (text)
  "Return the first nonempty request line in TEXT, skipping skill commands."
  (seq-find (lambda (line)
              (and (not (string-empty-p (string-trim line)))
                   (not (string-match-p
                         "\\`[ \t]*\\(?:/skill\\b\\|\\$[[:alnum:]_-]+[ \t]*\\'\\|<[^>]+>[ \t]*\\'\\)"
                         line))))
            (split-string text "\n")))

(defun pi-coding-agent--reading-hide-welcome ()
  "Hide the known welcome region at the first visible session message."
  ;; Only remove the known startup header; existing buffers may have been
  ;; created before these indexes were loaded.
  (unless pi-coding-agent--reading-welcome-hidden
   (let* ((header (concat (pi-coding-agent--format-startup-header) "\n"))
         (end (+ (point-min) (length header))))
    (when (and (<= end (point-max))
               (equal header (buffer-substring-no-properties (point-min) end)))
      (let ((inhibit-read-only t)) (delete-region (point-min) end))))
   (setq pi-coding-agent--reading-welcome-hidden t)))

(defun pi-coding-agent--reading-begin-turn (text)
  "Index the user boundary for TEXT and settle the preceding history turn."
  (when pi-coding-agent--reading-current
    (pi-coding-agent--reading-finish))
  (pi-coding-agent--reading-hide-welcome)
  (unless pi-coding-agent--reading-title
    (setq pi-coding-agent--reading-title
          (pi-coding-agent--reading-first-request text)))
  (let ((turn (pi-coding-agent--make-reading-turn
               :id pi-coding-agent--current-activity-turn
               :boundary (copy-marker (point-max))
               :state (if pi-coding-agent--reading-history 'complete 'running))))
    (push turn pi-coding-agent--reading-turns)
    (setq pi-coding-agent--reading-current turn)))

(defun pi-coding-agent--reading-current ()
  "Return the current turn, creating an orphan index when necessary."
  (or pi-coding-agent--reading-current
      (progn
        (pi-coding-agent--reading-hide-welcome)
        (let ((turn (pi-coding-agent--make-reading-turn
                   :id (pi-coding-agent--activity-current-turn)
                   :boundary (copy-marker (point-max)) :state 'running)))
        (push turn pi-coding-agent--reading-turns)
          (setq pi-coding-agent--reading-current turn)))))

(defun pi-coding-agent--reading-message-start ()
  "Start indexing a new assistant message without forgetting earlier text."
  (setf (pi-coding-agent--reading-turn-message-answer
         (pi-coding-agent--reading-current)) nil))

(defun pi-coding-agent--reading-run-start ()
  "Reset answer selection for a retry of the current user turn."
  (let ((turn (pi-coding-agent--reading-current)))
    (when (overlayp (pi-coding-agent--reading-turn-separator turn))
      (delete-overlay (pi-coding-agent--reading-turn-separator turn)))
    (setf (pi-coding-agent--reading-turn-answer turn) nil
          (pi-coding-agent--reading-turn-message-answer turn) nil
          (pi-coding-agent--reading-turn-state turn) 'running)
    (when (pi-coding-agent--reading-turn-start turn)
      (pi-coding-agent--reading-render turn))))

(defun pi-coding-agent--reading-text (text)
  "Index the first nonblank TEXT unit in this assistant message."
  (when (and (stringp text) (string-match-p "[^ \t\r\n]" text))
    (let ((turn (pi-coding-agent--reading-current)))
      (unless (pi-coding-agent--reading-turn-message-answer turn)
        (let ((marker (copy-marker
                       (or pi-coding-agent--streaming-marker (point-max)) t)))
          (setf (pi-coding-agent--reading-turn-message-answer turn) marker
                (pi-coding-agent--reading-turn-answer turn) marker)
          ;; The renderer inserts at the marker; freeze it just before insertion.
          (set-marker-insertion-type marker nil))))))

(defun pi-coding-agent--reading-text-inserted ()
  "Keep an indexed answer after decorations inserted at its left boundary."
  (when-let* ((turn pi-coding-agent--reading-current)
              (marker (pi-coding-agent--reading-turn-message-answer turn)))
    (set-marker-insertion-type marker t)))

(defun pi-coding-agent--reading-render (turn)
  "Rewrite only TURN's thinking projection."
  (let ((start (pi-coding-agent--reading-turn-start turn))
        (end (pi-coding-agent--reading-turn-end turn))
        (inhibit-read-only t))
    (when start
      (let ((saved (pi-coding-agent--capture-window-rewrite-states)))
        (unwind-protect
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (let* ((visible (eq (pi-coding-agent--thinking-display-mode) 'visible))
                 (expanded (or visible (pi-coding-agent--reading-turn-expanded turn)))
                 (running (eq (pi-coding-agent--reading-turn-state turn) 'running))
                 (label (if running "working"
                          (format "%d thoughts"
                                  (length (pi-coding-agent--reading-turn-thoughts turn)))))
                 (position (point)))
            (insert (pi-coding-agent--activity-row-text
                     (cond (expanded "▾") (running "◌") (t "▸"))
                     'thinking label) "\n")
            (make-text-button position (1- (point))
                              :type 'pi-coding-agent-activity-button
                              'action #'pi-coding-agent--reading-button
                              'pi-reading-turn turn 'pi-reading-index nil
                              'help-echo "TAB/RET/click: toggle thoughts"
                              'follow-link t)
            (when expanded
              (cl-loop for fragment in (pi-coding-agent--reading-turn-thoughts turn)
                       for index from 0 do
                       (let ((thought (plist-get fragment :text))
                             (position (point))
                             (selected (or visible
                                           (eql index (pi-coding-agent--reading-turn-selected turn)))))
                         (insert "  "
                                 (truncate-string-to-width
                                  (pi-coding-agent--activity-row-text
                                   (if selected "▾" "▸") 'thinking
                                   (plist-get fragment :preview))
                                  70 nil nil "…") "\n")
                         (make-text-button position (1- (point))
                                           :type 'pi-coding-agent-activity-button
                                           'action #'pi-coding-agent--reading-button
                                           'pi-reading-turn turn 'pi-reading-index index
                                           'help-echo "TAB/RET/click: toggle full thought"
                                           'follow-link t)
                         (when selected
                           (insert (pi-coding-agent--thinking-blockquote-text
                                    (pi-coding-agent--thinking-normalize-text thought)) "\n"))))
              (when (and (eq turn pi-coding-agent--reading-current)
                         pi-coding-agent--reading-chunks)
                (let ((position (point))
                      (text (apply #'concat (reverse pi-coding-agent--reading-chunks)))
                      (selected (eq (pi-coding-agent--reading-turn-selected turn) 'live)))
                  (insert "  " (pi-coding-agent--activity-row-text
                                (if selected "▾" "◌") 'thinking "working") "\n")
                  (make-text-button position (1- (point))
                                    :type 'pi-coding-agent-activity-button
                                    'action #'pi-coding-agent--reading-button
                                    'pi-reading-turn turn 'pi-reading-index 'live
                                    'help-echo "TAB/RET/click: toggle live thought"
                                    'follow-link t)
                  (when selected
                    (insert (pi-coding-agent--thinking-blockquote-text
                             (pi-coding-agent--thinking-normalize-text text)) "\n"))))))
          (set-marker end (point)))
          (pi-coding-agent--restore-window-rewrite-states (current-buffer) saved))))))

(defun pi-coding-agent--reading-button (button)
  "Toggle the turn or thought identified by BUTTON."
  (let ((turn (button-get button 'pi-reading-turn))
        (index (button-get button 'pi-reading-index)))
    (if index
        (setf (pi-coding-agent--reading-turn-selected turn)
              (unless (eql index (pi-coding-agent--reading-turn-selected turn)) index))
      (setf (pi-coding-agent--reading-turn-expanded turn)
            (not (pi-coding-agent--reading-turn-expanded turn))))
    (pi-coding-agent--reading-render turn)))

(defun pi-coding-agent--reading-toggle ()
  "Handle TAB on a grouped thought, returning non-nil when handled."
  (when-let* ((button (button-at (point)))
              ((button-get button 'pi-reading-turn)))
    (pi-coding-agent--reading-button button)
    t))

(defun pi-coding-agent--reading-thought-anchor ()
  "Place the thinking group at the first nonempty thought."
  (let ((turn (pi-coding-agent--reading-current)))
    (unless (pi-coding-agent--reading-turn-start turn)
      (save-excursion
        (goto-char (or pi-coding-agent--streaming-marker (point-max)))
        (let ((inhibit-read-only t))
          (pi-coding-agent--ensure-blank-line-before-block))
        (setf (pi-coding-agent--reading-turn-start turn) (copy-marker (point))
              (pi-coding-agent--reading-turn-end turn) (copy-marker (point)))
        (pi-coding-agent--reading-render turn)))))

(defun pi-coding-agent--reading-thought-delta (delta)
  "Collect DELTA without rewriting the stable collapsed working row."
  (push delta pi-coding-agent--reading-chunks)
  (when (string-match-p "[^ \t\r\n]" delta)
    (pi-coding-agent--reading-thought-anchor))
  (when (pi-coding-agent--reading-turn-expanded (pi-coding-agent--reading-current))
    (pi-coding-agent--reading-render pi-coding-agent--reading-current)))

(defun pi-coding-agent--reading-thought-end (text)
  "Index authoritative thought TEXT, falling back to streamed chunks."
  (let* ((text (if (and (stringp text) (not (string-empty-p text))) text
                 (apply #'concat (nreverse pi-coding-agent--reading-chunks))))
         (turn (pi-coding-agent--reading-current)))
    (setq pi-coding-agent--reading-chunks nil)
    (when (string-match-p "[^ \t\r\n]" text)
      (when (eq (pi-coding-agent--reading-turn-selected turn) 'live)
        (setf (pi-coding-agent--reading-turn-selected turn)
              (length (pi-coding-agent--reading-turn-thoughts turn))))
      (let ((cell (list (list :text text
                             :locator (list :turn (pi-coding-agent--reading-turn-id turn)
                                            :thought (length (pi-coding-agent--reading-turn-thoughts turn)))
                             :preview (pi-coding-agent--single-line-preview
                                       (or (pi-coding-agent--first-nonempty-line text) ""))))))
        (if (pi-coding-agent--reading-turn-thought-tail turn)
            (setcdr (pi-coding-agent--reading-turn-thought-tail turn) cell)
          (setf (pi-coding-agent--reading-turn-thoughts turn) cell))
        (setf (pi-coding-agent--reading-turn-thought-tail turn) cell))
      (pi-coding-agent--reading-thought-anchor)
      (when (pi-coding-agent--reading-turn-expanded turn)
        (pi-coding-agent--reading-render turn)))))

(defun pi-coding-agent--reading-finish (&optional interrupted)
  "Settle the current run, marking it INTERRUPTED when appropriate."
  (when-let* ((turn pi-coding-agent--reading-current))
    (when pi-coding-agent--reading-chunks
      (pi-coding-agent--reading-thought-end nil))
    (setf (pi-coding-agent--reading-turn-state turn)
          (if (or interrupted
                  (eq (pi-coding-agent--reading-turn-state turn) 'interrupted))
              'interrupted 'complete))
    (pi-coding-agent--reading-render turn)
    (when (overlayp (pi-coding-agent--reading-turn-separator turn))
      (delete-overlay (pi-coding-agent--reading-turn-separator turn)))
    (when-let* ((answer (pi-coding-agent--reading-turn-answer turn)))
      (let ((overlay (make-overlay answer answer nil t nil)))
        (overlay-put overlay 'before-string
                     (propertize
                      (if (eq (pi-coding-agent--reading-turn-state turn) 'interrupted)
                          "回复中断\n" "答复\n") 'face 'shadow))
        (setf (pi-coding-agent--reading-turn-separator turn) overlay)))))

(defun pi-coding-agent--reading-call (function)
  "Call FUNCTION in the bound Output, preserving Input focus and draft."
  (let* ((input (derived-mode-p 'pi-coding-agent-input-mode))
         (chat (pi-coding-agent--get-chat-buffer))
         (window (and (buffer-live-p chat)
                      (if (eq (window-buffer (selected-window)) chat)
                          (selected-window) (get-buffer-window chat)))))
    (unless (window-live-p window)
      (user-error "Pi: No bound Output window"))
    (if input
        (let ((origin (selected-window)))
          (unwind-protect
              (progn (select-window window) (funcall function))
            (when (window-live-p origin) (select-window origin))))
      (with-selected-window window (funcall function)))))

(defun pi-coding-agent--reading-goto-answer (latest)
  "Navigate to the indexed answer, choosing the latest turn when LATEST."
  (pi-coding-agent--reading-call
   (lambda ()
     (let* ((turn (if latest (car pi-coding-agent--reading-turns)
                    (seq-find (lambda (item)
                                (<= (pi-coding-agent--reading-turn-boundary item) (point)))
                              pi-coding-agent--reading-turns)))
            (answer (and turn (pi-coding-agent--reading-turn-answer turn))))
       (if (not (and (markerp answer) (marker-position answer)))
           (message "Pi: 本轮尚无答复正文")
         (push-mark (point) t)
         (goto-char answer)
         (set-window-start (selected-window) answer)
         (set-window-parameter (selected-window) 'pi-reading-follow nil)
         (message "Pi: %s"
                  (pcase (pi-coding-agent--reading-turn-state turn)
                    ('running "进行中的正文") ('interrupted "回复中断") (_ "答复"))))))))

(defun pi-coding-agent-goto-turn-answer ()
  "Go to the current user turn's answer; use the mark ring to return."
  (interactive) (pi-coding-agent--reading-goto-answer nil))

(defun pi-coding-agent-goto-latest-answer ()
  "Go to the latest user turn's answer."
  (interactive) (pi-coding-agent--reading-goto-answer t))

(defun pi-coding-agent-resume-following ()
  "Resume following only the selected or bound Output window."
  (interactive)
  (pi-coding-agent--reading-call
   (lambda ()
     (push-mark (point) t)
     (goto-char (point-max))
     (set-window-parameter (selected-window) 'pi-reading-follow t)
     (set-window-parameter (selected-window) 'pi-reading-new nil)
     (recenter -1)
     (force-mode-line-update))))

(defun pi-coding-agent--reading-content-event ()
  "Update visible windows before a real content event is projected."
  (cl-incf pi-coding-agent--reading-generation)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (let ((following (>= (window-point window) (1- (point-max)))))
      (unless (eq (window-parameter window 'pi-reading-chat) (current-buffer))
        (set-window-parameter window 'pi-reading-chat (current-buffer))
        (set-window-parameter window 'pi-reading-new nil))
      (set-window-parameter window 'pi-reading-follow following)
      (when following
        (set-window-parameter window 'pi-reading-seen pi-coding-agent--reading-generation))
      (unless following
        (unless (window-parameter window 'pi-reading-new)
          (set-window-parameter window 'pi-reading-new t)
          (force-mode-line-update))))))

(defun pi-coding-agent-reading-status (&optional window)
  "Return the static reading hint for WINDOW's mode line."
  (let ((window (or window (selected-window))))
    (when (and pi-coding-agent-reading-hints
               (eq (window-buffer window) (current-buffer)))
      (if (>= (window-point window) (1- (point-max)))
          (progn
            (set-window-parameter window 'pi-reading-new nil)
            (set-window-parameter window 'pi-reading-seen pi-coding-agent--reading-generation)
            nil)
        (if (and (eq (window-parameter window 'pi-reading-chat) (current-buffer))
                 (window-parameter window 'pi-reading-new))
            "阅读历史 · 有新输出" "阅读历史")))))

(defun pi-coding-agent--reading-window-rebound (frame)
  "Clear reading state for rebound windows on FRAME.
The global `window-buffer-change-functions' hook receives a frame."
  (dolist (window (window-list frame 'no-minibuffer))
    (unless (eq (window-buffer window) (window-parameter window 'pi-reading-chat))
      (dolist (parameter '(pi-reading-chat pi-reading-follow pi-reading-new pi-reading-seen))
        (set-window-parameter window parameter nil)))))

(add-hook 'window-buffer-change-functions #'pi-coding-agent--reading-window-rebound)

(defun pi-coding-agent--reading-failed-tool (direction)
  "Visit the next failed tool in DIRECTION without wrapping."
  (pi-coding-agent--reading-call
   (lambda ()
     (let ((position (point)) candidates target)
       (dolist (turn (reverse pi-coding-agent--reading-turns))
         (when-let* ((record (pi-coding-agent--activity-record-for-turn
                             (pi-coding-agent--reading-turn-id turn)))
                     (start (pi-coding-agent--activity-record-start-marker record)))
           (dolist (tool (pi-coding-agent--activity-record-tools record))
             (when (eq (plist-get tool :status) 'failed)
               (push (list record tool start) candidates)))))
       (setq candidates (nreverse candidates))
       (let* ((current-id (get-text-property position 'pi-coding-agent-activity-tool-call-id))
              (current (seq-position candidates current-id
                                     (lambda (entry id)
                                       (equal (plist-get (cadr entry) :tool-call-id) id)))))
         (setq target
               (if current
                   (nth (if (> direction 0) (1+ current) (max 0 (1- current))) candidates)
                 (seq-find (lambda (entry)
                             (if (> direction 0) (> (nth 2 entry) position)
                               (< (nth 2 entry) position)))
                           (if (> direction 0) candidates (reverse candidates)))))
         (when (and current (= current 0) (< direction 0)) (setq target nil)))
       (if (null target)
           (message "Pi: 此方向没有失败的工具调用")
         (push-mark position t)
         (let ((record (car target)) (tool (cadr target)))
           (pi-coding-agent--reading-expand-tools record)
           (pi-coding-agent--activity-render-record record)
           (goto-char (pi-coding-agent--activity-record-start-marker record))
           (when-let* ((found (text-property-search-forward
                              'pi-coding-agent-activity-tool-call-id
                              (plist-get tool :tool-call-id) t)))
             (goto-char (prop-match-beginning found)))
           (pi-coding-agent--activity-toggle-tool-detail record tool)))))))

(defun pi-coding-agent-next-failed-tool ()
  "Visit the next failed tool and open its existing detail view."
  (interactive) (pi-coding-agent--reading-failed-tool 1))

(defun pi-coding-agent-previous-failed-tool ()
  "Visit the previous failed tool and open its existing detail view."
  (interactive) (pi-coding-agent--reading-failed-tool -1))

(declare-function pi-coding-agent--reading-expand-tools "pi-coding-agent-render" (record))

(provide 'pi-coding-agent-reading)
;;; pi-coding-agent-reading.el ends here
