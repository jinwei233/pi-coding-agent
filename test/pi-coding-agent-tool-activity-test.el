;;; pi-coding-agent-tool-activity-test.el --- Hidden tool activity -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract tests for `pi-coding-agent-tool-activity-display' = `hidden':
;; successful tools occupy no visible lines, TAB reveals one line per tool
;; for the current turn, reveal lines open the full detail, running and
;; failed tools stay visible, and the header line counts hidden activity.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'isearch)
(require 'pi-coding-agent)

;;;; Helpers

(defmacro pi-coding-agent-tool-activity-test--with-chat (&rest body)
  "Run BODY in a fresh chat buffer with hidden tool activity."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (pi-coding-agent-chat-mode)
     (pi-coding-agent--set-chat-session-identity "/tmp/pi-tool-activity/")
     (setq-local pi-coding-agent--tool-activity-display 'hidden)
     ,@body))

(defun pi-coding-agent-tool-activity-test--visible ()
  "Return the plain visible text of the current buffer."
  (substring-no-properties
   (pi-coding-agent--visible-text (point-min) (point-max))))

(defun pi-coding-agent-tool-activity-test--render-tool
    (id name args content &optional details error)
  "Render one completed tool with ID, NAME, ARGS, CONTENT and ERROR."
  (pi-coding-agent--handle-display-event
   (list :type "tool_execution_start"
         :toolCallId id
         :toolName name
         :args args))
  (pi-coding-agent--handle-display-event
   (list :type "tool_execution_end"
         :toolCallId id
         :toolName name
         :result (list :content (vector (list :type "text" :text content))
                       :details details)
         :isError error)))

(defun pi-coding-agent-tool-activity-test--history
    (&rest tool-specs)
  "Render history with TOOL-SPECS, each (NAME ARGS CONTENT) or (NAME ARGS CONTENT ERROR)."
  (let* ((messages nil)
         (index 0))
    (push (list :role "user" :content "please run things"
                :timestamp 1704067200000)
          messages)
    (dolist (spec tool-specs)
      (setq index (1+ index))
      (let* ((id (format "call-%d" index))
             (name (nth 0 spec))
             (args (nth 1 spec))
             (content (nth 2 spec))
             (error (nth 3 spec)))
        (push (list :role "assistant"
                    :content (vector (list :type "toolCall" :id id
                                           :name name :arguments args))
                    :timestamp 1704067200000)
              messages)
        (push (list :role "toolResult" :toolCallId id :toolName name
                    :content (vector (list :type "text" :text content))
                    :isError (and error t)
                    :timestamp 1704067201000)
              messages)))
    (pi-coding-agent--display-session-history (vconcat (nreverse messages))
                                              (current-buffer))))

(defun pi-coding-agent-tool-activity-test--goto-user-text ()
  "Move point into the user turn body."
  (goto-char (point-min))
  (search-forward "please run things")
  (goto-char (match-beginning 0)))

(defun pi-coding-agent-tool-activity-test--reveal-lines ()
  "Return the visible reveal lines of the current buffer."
  (seq-filter (lambda (line)
                (string-match-p
                 "\\`\\(bash\\|read\\|write\\|edit\\|grep\\|search\\|subagent\\|custom\\) \u00b7"
                 line))
              (split-string (pi-coding-agent-tool-activity-test--visible) "\n")))

;;;; 1.1 Successful tools occupy no visible line

(ert-deftest pi-coding-agent-test-tool-activity-live-success-is-invisible ()
  "A live successful tool adds no visible line."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run the tests")
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Checking now.")
    (pi-coding-agent-tool-activity-test--render-tool
     "c1" "bash" '(:command "npm test") "alpha\nbeta\ngamma")
    (pi-coding-agent--display-message-delta "Done.")
    (pi-coding-agent--finish-assistant-text)
    (let ((visible (pi-coding-agent-tool-activity-test--visible))
          (raw (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "npm test" visible))
      (should-not (string-match-p "alpha" visible))
      (should-not (string-match-p "lines" visible))
      (should (string-match-p "Checking now\\.\n\nDone\\." visible))
      ;; Structure and metadata survive for reveal and detail.
      (should (string-match-p "npm test" raw))
      (should (= 1 pi-coding-agent--hidden-tool-count)))))

(ert-deftest pi-coding-agent-test-tool-activity-history-success-is-invisible ()
  "History replay and rebuild keep successful tools invisible."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "npm test") "alpha\nbeta")
     '("read" (:path "/tmp/pi-tool-activity/src/a.el") "line1"))
    (let ((before (pi-coding-agent-tool-activity-test--visible)))
      (should-not (string-match-p "npm test" before))
      (should-not (string-match-p "line1" before))
      (should (= 2 pi-coding-agent--hidden-tool-count))
      ;; A canonical rebuild reproduces the same invisible state.
      (pi-coding-agent--rerender-canonical-history)
      (should (equal before (pi-coding-agent-tool-activity-test--visible)))
      (should (= 2 pi-coding-agent--hidden-tool-count)))))

(ert-deftest pi-coding-agent-test-tool-activity-running-and-failed-stay-visible ()
  "Running and failed tools keep one visible line each."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run the tests")
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "c1" :toolName "bash"
       :args (:command "npm test")))
    (let ((running (pi-coding-agent-tool-activity-test--visible)))
      (should (string-match-p "\u25b8 bash \u00b7 running" running)))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_end" :toolCallId "c1" :toolName "bash"
       :result (:content ((:type "text" :text "boom\nsecond line")))
       :isError t))
    (let ((failed (pi-coding-agent-tool-activity-test--visible)))
      (should (string-match-p "failed \u00b7 boom" failed))
      (should-not (string-match-p "second line" failed))
      (should-not (string-match-p "running" failed)))
    ;; Failure is diagnosable: TAB on the failure line expands the detail.
    (goto-char (point-min))
    (search-forward "failed \u00b7 boom")
    (pi-coding-agent-toggle-tool-section)
    (should (string-match-p "second line"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

;;;; 1.2 Hiding keeps spacing and Markdown intact

(ert-deftest pi-coding-agent-test-tool-activity-hiding-keeps-spacing ()
  "Hidden tool blocks add no blank lines and do not break later Markdown."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run the tests")
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "First.")
    (pi-coding-agent-tool-activity-test--render-tool
     "c1" "bash" '(:command "one") "a\nb")
    (pi-coding-agent-tool-activity-test--render-tool
     "c2" "bash" '(:command "two") "c\nd")
    (pi-coding-agent--display-message-delta "Second.")
    (let ((visible (pi-coding-agent-tool-activity-test--visible)))
      (should (string-match-p "First\\.\n\nSecond\\." visible))
      (should-not (string-match-p "\n\n\n" visible)))))

(ert-deftest pi-coding-agent-test-tool-activity-only-tool-turn-keeps-one-blank ()
  "A turn containing only tools does not stretch the turn gap."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "one") "a")
     '("bash" (:command "two") "b"))
    (let* ((visible (pi-coding-agent-tool-activity-test--visible))
           (tail (save-match-data
                   (if (string-match "please run things" visible)
                       (substring visible (match-end 0))
                     visible))))
      ;; No stray blank lines after the turn text (the startup header has its
      ;; own pre-existing spacing).
      (should-not (string-match-p "\n\n\n" tail))
      (should (= 2 pi-coding-agent--hidden-tool-count)))))

(ert-deftest pi-coding-agent-test-tool-activity-hidden-summary-cannot-break-markdown ()
  "A hidden fence-like summary cannot swallow later Markdown."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "false") "```js | a | b" t))
    (pi-coding-agent--display-user-message "another turn")
    (should-not (string-match-p "\\`\n*```" (pi-coding-agent-tool-activity-test--visible)))
    (should (string-match-p "another turn"
                            (pi-coding-agent-tool-activity-test--visible)))))

;;;; 1.3 Copy and search semantics

(ert-deftest pi-coding-agent-test-tool-activity-copy-excludes-hidden-tools ()
  "Visible copy omits hidden tool text while raw copy keeps it."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "npm test") "SECRET_OUTPUT"))
    (let ((copy (filter-buffer-substring (point-min) (point-max))))
      (should (string-match-p "please run things" copy))
      (should-not (string-match-p "npm test" copy))
      (should-not (string-match-p "SECRET_OUTPUT" copy)))
    (let ((pi-coding-agent-copy-raw-markdown t))
      (let ((raw-copy (filter-buffer-substring (point-min) (point-max))))
        (should (string-match-p "npm test" raw-copy))
        (should (string-match-p "SECRET_OUTPUT" raw-copy))))))

(ert-deftest pi-coding-agent-test-tool-activity-isearch-skips-hidden-tools ()
  "isearch does not stop on hidden tool text."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "npm test") "SECRET_OUTPUT"))
    (let ((header (save-excursion
                    (goto-char (point-min))
                    (search-forward "npm test")
                    (line-beginning-position))))
      (should (isearch-range-invisible header (+ header 5)))
      (should-not search-invisible))))

;;;; 1.4 Turn-level TAB ladder

(ert-deftest pi-coding-agent-test-tool-activity-tab-ladder ()
  "TAB reveals the turn, expands a reveal line, then hides the turn again."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "npm test") "alpha\nbeta"))
    (pi-coding-agent-tool-activity-test--goto-user-text)
    (pi-coding-agent-toggle-tool-section)
    (should (equal '("bash \u00b7 npm \u00b7 2 lines \u00b7 alpha")
                   (pi-coding-agent-tool-activity-test--reveal-lines)))
    (should (= 0 pi-coding-agent--hidden-tool-count))
    ;; Second TAB on the reveal line opens the detail.
    (goto-char (point-min))
    (search-forward "bash \u00b7")
    (pi-coding-agent-toggle-tool-section)
    (should (string-match-p "alpha\nbeta"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))
    ;; Third TAB on the reveal line collapses the detail again.
    (goto-char (point-min))
    (search-forward "bash \u00b7")
    (pi-coding-agent-toggle-tool-section)
    (should-not (string-match-p "alpha\nbeta"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
    ;; TAB in the turn body hides the whole turn again.
    (pi-coding-agent-tool-activity-test--goto-user-text)
    (pi-coding-agent-toggle-tool-section)
    (should-not (pi-coding-agent-tool-activity-test--reveal-lines))
    (should (= 1 pi-coding-agent--hidden-tool-count))))

(ert-deftest pi-coding-agent-test-tool-activity-tab-does-not-fall-back-to-outline ()
  "TAB prefers the turn reveal over `outline-cycle'."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "npm test") "alpha"))
    (let ((outline-called nil))
      (cl-letf (((symbol-function 'outline-cycle)
                 (lambda (&rest _) (setq outline-called t))))
        (pi-coding-agent-tool-activity-test--goto-user-text)
        (pi-coding-agent-toggle-tool-section)
        (should-not outline-called)
        (should (pi-coding-agent-tool-activity-test--reveal-lines))))))

;;;; 1.5 Reveal line shape

(ert-deftest pi-coding-agent-test-tool-activity-reveal-line-shape ()
  "The reveal line is one short line without the command echo."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "cd /tmp && npm test -- --watch=false") "alpha")
     '("edit" (:path "/tmp/pi-tool-activity/src/nested/deep/file.el") "done"
       (:diff "+a\n-b")))
    (pi-coding-agent-tool-activity-test--goto-user-text)
    (pi-coding-agent-toggle-tool-section)
    (let ((lines (pi-coding-agent-tool-activity-test--reveal-lines)))
      (should (= 2 (length lines)))
      (dolist (line lines)
        (should (<= (string-width line)
                    pi-coding-agent--single-line-preview-columns)))
      ;; Paths are project-relative, not absolute.
      (should (seq-some (lambda (line) (string-match-p "src/nested/deep/file\\.el" line))
                        lines))
      (should-not (seq-some (lambda (line) (string-match-p "/tmp/pi-tool-activity" line))
                            lines))
      ;; No command echo and no escaped newlines.
      ;; Only a one-token hint, never the full command line.
      (should (seq-some (lambda (line) (string-match-p "bash \u00b7 cd \u00b7" line)) lines))
      (should-not (seq-some (lambda (line) (string-match-p "npm test -- --watch" line))
                            lines))
      (should-not (seq-some (lambda (line) (string-match-p "\\\\n" line)) lines)))))

;;;; 1.6 Reveal line is a whole-line button

(ert-deftest pi-coding-agent-test-tool-activity-reveal-line-is-button ()
  "The whole reveal line toggles detail from any character."
  (dolist (offset '(0 3 10))
    (pi-coding-agent-tool-activity-test--with-chat
      (pi-coding-agent-tool-activity-test--history
       '("bash" (:command "npm test") "alpha\nbeta"))
      (pi-coding-agent-tool-activity-test--goto-user-text)
      (pi-coding-agent-toggle-tool-section)
      (goto-char (point-min))
      (search-forward "bash \u00b7")
      (goto-char (+ (line-beginning-position) offset))
      (should (button-at (point)))
      (pi-coding-agent-toggle-tool-section)
      (should (string-match-p "alpha\nbeta"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest pi-coding-agent-test-tool-activity-reveal-button-metadata ()
  "Reveal buttons carry toggle metadata and use no button face."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "npm test") "alpha"))
    (pi-coding-agent-tool-activity-test--goto-user-text)
    (pi-coding-agent-toggle-tool-section)
    (goto-char (point-min))
    (search-forward "bash \u00b7")
    (let* ((button (button-at (line-beginning-position)))
           (line-end (line-end-position)))
      (should button)
      (should (button-get button 'pi-coding-agent-tool-toggle))
      (should (equal "call-1" (button-get button 'pi-coding-agent-tool-call-id)))
      (should (button-at line-end))
      (should-not (get-text-property (line-beginning-position) 'face)))))

;;;; 1.8 Header-line counter

(ert-deftest pi-coding-agent-test-tool-activity-header-counter-format ()
  "The header segment is empty at zero and compact otherwise."
  (should (equal "" (pi-coding-agent--header-format-tool-activity 0)))
  (should (equal "" (pi-coding-agent--header-format-tool-activity nil)))
  (let ((rendered (pi-coding-agent--header-format-tool-activity 5)))
    (should (string-match-p "tools 5" rendered))))

(ert-deftest pi-coding-agent-test-tool-activity-header-counter-tracks-reveal ()
  "The counter follows reveal, hide and history rebuild."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "npm test") "alpha"))
    (should (= 1 pi-coding-agent--hidden-tool-count))
    (pi-coding-agent-tool-activity-test--goto-user-text)
    (pi-coding-agent-toggle-tool-section)
    (should (= 0 pi-coding-agent--hidden-tool-count))
    (pi-coding-agent-tool-activity-test--goto-user-text)
    (pi-coding-agent-toggle-tool-section)
    (should (= 1 pi-coding-agent--hidden-tool-count))
    (pi-coding-agent--rerender-canonical-history)
    (should (= 1 pi-coding-agent--hidden-tool-count))))

(ert-deftest pi-coding-agent-test-tool-activity-counter-scans-headings-once ()
  "Counting hidden tools reuses one user-heading scan for every tool."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "one") "alpha")
     '("read" (:path "/tmp/pi-tool-activity/a.el") "beta")
     '("write" (:path "/tmp/pi-tool-activity/b.el" :content "gamma") "done"))
    (let ((original (symbol-function 'pi-coding-agent--collect-you-headings))
          (calls 0))
      (cl-letf (((symbol-function 'pi-coding-agent--collect-you-headings)
                 (lambda ()
                   (setq calls (1+ calls))
                   (funcall original))))
        (pi-coding-agent--update-hidden-tool-count))
      (should (= 1 calls))
      (should (= 3 pi-coding-agent--hidden-tool-count)))))

;;;; 1.9 Cold history

(ert-deftest pi-coding-agent-test-tool-activity-cold-block-is-invisible ()
  "Cooled tool blocks keep no visible text but keep metadata and reveal."
  (pi-coding-agent-tool-activity-test--with-chat
    (let ((pi-coding-agent-hot-tail-turn-count 0))
      (pi-coding-agent-tool-activity-test--history
       '("read" (:path "/tmp/pi-tool-activity/src/a.el") "SECRET_BODY")))
    (let ((visible (pi-coding-agent-tool-activity-test--visible)))
      (should-not (string-match-p "SECRET_BODY" visible))
      (should-not (string-match-p "src/a.el" visible)))
    (should (= 0 (seq-count (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                            (overlays-in (point-min) (point-max)))))
    (should (= 1 pi-coding-agent--hidden-tool-count))
    ;; Reveal rebuilds the reduced line from the kept text and metadata.
    (pi-coding-agent-tool-activity-test--goto-user-text)
    (pi-coding-agent-toggle-tool-section)
    (should (string-match-p "read \u00b7 src/a\\.el \u00b7 1 line"
                            (pi-coding-agent-tool-activity-test--visible)))
    ;; Detail opens in a read-only buffer, then clean up.
    (goto-char (point-min))
    (search-forward "read \u00b7")
    (let ((detail nil))
      (pi-coding-agent-toggle-tool-section)
      (setq detail (gethash "call-1" pi-coding-agent--tool-detail-buffers))
      (should (buffer-live-p detail))
      (when (buffer-live-p detail) (kill-buffer detail))
      (remhash "call-1" pi-coding-agent--tool-detail-buffers))))

(ert-deftest pi-coding-agent-test-tool-activity-orphaned-window-can-be-revealed ()
  "Activity before any user heading still reveals through a synthetic turn."
  (pi-coding-agent-tool-activity-test--with-chat
    ;; History window without a user message, as a compacted transcript can be.
    (pi-coding-agent--display-session-history
     (vector
      (list :role "assistant"
            :content (vector (list :type "toolCall" :id "call-1"
                                   :name "bash" :arguments '(:command "npm test")))
            :timestamp 1704067200000)
      (list :role "toolResult" :toolCallId "call-1" :toolName "bash"
            :content (vector (list :type "text" :text "alpha"))
            :isError nil :timestamp 1704067201000))
     (current-buffer))
    (should (= 1 pi-coding-agent--hidden-tool-count))
    (should (equal -1 (pi-coding-agent--tool-activity-turn-ordinal (point-min))))
    (goto-char (point-min))
    (pi-coding-agent-toggle-tool-section)
    (should (= 1 (length (pi-coding-agent-tool-activity-test--reveal-lines))))
    (should (= 0 pi-coding-agent--hidden-tool-count))))

;;;; 1.10 Display-mode fallback

(ert-deftest pi-coding-agent-test-tool-activity-summary-mode-restores-header ()
  "The summary mode keeps the established header plus summary form."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--set-chat-session-identity "/tmp/pi-tool-activity/")
    (setq-local pi-coding-agent--tool-activity-display 'summary)
    (pi-coding-agent-tool-activity-test--history
     '("read" (:path "/tmp/pi-tool-activity/src/a.el") "line1"))
    (let ((visible (pi-coding-agent-tool-activity-test--visible)))
      (should (string-match-p "read /tmp/pi-tool-activity/src/a\\.el" visible))
      (should (string-match-p "1 line" visible)))
    (should (= 0 pi-coding-agent--hidden-tool-count))
    ;; Detail still works in summary mode.
    (goto-char (point-min))
    (search-forward "1 line")
    (pi-coding-agent-toggle-tool-section)
    (should (string-match-p "line1"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

;;;; 1.11 Plural labels

(ert-deftest pi-coding-agent-test-tool-activity-line-count-label ()
  "Line count labels use correct singular and plural forms."
  (should (equal "1 line" (pi-coding-agent--line-count-label 1)))
  (should (equal "2 lines" (pi-coding-agent--line-count-label 2)))
  (should (equal "0 lines" (pi-coding-agent--line-count-label 0))))

;;;; 1.12 Navigation is unaffected

(ert-deftest pi-coding-agent-test-tool-activity-navigation-unchanged ()
  "Turn navigation, fork ordinals and hot-tail boundaries ignore hidden tools."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--history
     '("bash" (:command "npm test") "alpha"))
    (pi-coding-agent--display-user-message "second turn")
    (let ((headings (pi-coding-agent--collect-you-headings)))
      (should (= 2 (length headings)))
      (goto-char (point-max))
      (should (= 1 (pi-coding-agent--user-turn-index-at-point headings)))
      (goto-char (car headings))
      (should (= 0 (pi-coding-agent--user-turn-index-at-point headings))))
    (let ((pi-coding-agent-hot-tail-turn-count 1))
      (pi-coding-agent--update-hot-tail-boundary)
      (goto-char (marker-position pi-coding-agent--hot-tail-start))
      (should (pi-coding-agent--at-turn-heading-p)))))

(provide 'pi-coding-agent-tool-activity-test)
;;; pi-coding-agent-tool-activity-test.el ends here
