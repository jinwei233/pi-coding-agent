;;; pi-coding-agent-transcript-test.el --- Quiet transcript contract -*- lexical-binding: t; -*-

;;; Commentary:

;; Display contract for the streamlined chat transcript: the `❯' user turn
;; marker, hidden structural headings, one-line tool summaries, the whole-line
;; summary button, the `Think · <preview>' stub, and the interaction, copy and
;; search semantics that must survive that presentation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'isearch)
(require 'pi-coding-agent)

;;;; Helpers

(defconst pi-coding-agent-transcript-test--marker "❯")

(defmacro pi-coding-agent-transcript-test--with-chat (&rest body)
  "Run BODY in a fresh chat buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (pi-coding-agent-chat-mode)
     (pi-coding-agent--set-chat-session-identity "/tmp/pi-transcript/")
     ,@body))

(defun pi-coding-agent-transcript-test--visible ()
  "Return the visible text of the current buffer."
  (pi-coding-agent--visible-text (point-min) (point-max)))

(defun pi-coding-agent-transcript-test--visible-copy (beg end)
  "Return the copy output for BEG..END."
  (filter-buffer-substring beg end))

(defun pi-coding-agent-transcript-test--messages (&rest specs)
  "Return canonical messages built from SPECS.
Each spec is (ROLE CONTENT) where ROLE is `user', `assistant' or
`toolResult'; see `pi-coding-agent-transcript-test--tool-messages' for the
single tool call helper."
  (vconcat
   (mapcar (lambda (spec)
             (pcase-let ((`(,role ,content) spec))
               (list :role role
                     :content content
                     :timestamp 1704067200000)))
           specs)))

(defun pi-coding-agent-transcript-test--tool-messages
    (name args content &optional details error)
  "Return messages for one completed tool NAME with ARGS and CONTENT."
  (vector
   (list :role "assistant"
         :content (vector (list :type "toolCall" :id "call-1"
                                :name name :arguments args))
         :timestamp 1704067200000)
   (list :role "toolResult" :toolCallId "call-1" :toolName name
         :content (vector (list :type "text" :text content))
         :details details
         :isError (and error t)
         :timestamp 1704067201000)))

(defmacro pi-coding-agent-transcript-test--with-history (messages &rest body)
  "Render MESSAGES as history in a chat buffer, then run BODY."
  (declare (indent 1) (debug t))
  `(pi-coding-agent-transcript-test--with-chat
     (pi-coding-agent--display-session-history ,messages (current-buffer))
     ,@body))

(defun pi-coding-agent-transcript-test--render-live-tool
    (name args content &optional details error)
  "Render one live completed tool in the current chat buffer."
  (setq pi-coding-agent--assistant-header-shown nil)
  (pi-coding-agent--handle-display-event '(:type "agent_start"))
  (pi-coding-agent--handle-display-event
   (list :type "tool_execution_start"
         :toolCallId "call-1"
         :toolName name
         :args args))
  (pi-coding-agent--handle-display-event
   (list :type "tool_execution_end"
         :toolCallId "call-1"
         :toolName name
         :result (list :content (vector (list :type "text" :text content))
                       :details details)
         :isError error)))

(defun pi-coding-agent-transcript-test--block-text (start end)
  "Return plain text between START and END."
  (buffer-substring-no-properties start end))

(defun pi-coding-agent-transcript-test--summary-button ()
  "Return the first summary toggle button in the buffer, or nil."
  (next-button (point-min)))

(defun pi-coding-agent-transcript-test--heading-pos (marker)
  "Return the buffer position of the hidden heading line containing MARKER."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward marker nil t)
      (line-beginning-position))))

(defun pi-coding-agent-transcript-test--hidden-structure-at-p (pos)
  "Return non-nil when POS carries hidden turn structure."
  (and (eq t (get-text-property
              pos pi-coding-agent--hidden-turn-structure-property))
       (invisible-p (get-text-property pos 'invisible))))

(defun pi-coding-agent-transcript-test--parsed-block-type (pos)
  "Return the nearest Markdown block type around POS, or nil without treesit."
  (when (treesit-parser-list)
    (let ((types '(("paragraph") ("setext_heading") ("atx_heading")
                   ("fenced_code_block") ("indented_code_block")
                   ("block_quote") ("list") ("table") ("pipe_table"))))
      (let ((node (treesit-node-at pos)))
        (while (and node
                    (not (seq-some (lambda (entry) (equal (car entry)
                                                          (treesit-node-type node)))
                                   types)))
          (setq node (treesit-node-parent node)))
        (and node (treesit-node-type node))))))

;;;; 1.1 Turn marker in live and history rendering

(ert-deftest pi-coding-agent-test-transcript-live-user-turn-uses-marker-line ()
  "A live user turn shows a bare marker line instead of You/Assistant headings."
  (pi-coding-agent-transcript-test--with-chat
    (pi-coding-agent--display-user-message "Hello there")
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Hi.")
    (pi-coding-agent--finish-assistant-text)
    (let ((visible (pi-coding-agent-transcript-test--visible))
          (raw (buffer-string)))
      (should (equal (concat "\n" pi-coding-agent-transcript-test--marker
                             "\nHello there\n\nHi.")
                     (string-trim-right visible)))
      (should-not (string-match-p "^You$" visible))
      (should-not (string-match-p "^Assistant$" visible))
      (should-not (string-match-p "=+" visible))
      ;; Structure stays in the buffer for navigation.
      (should (string-match-p "You\n===" raw))
      (should (string-match-p "Assistant\n=========" raw)))))

(ert-deftest pi-coding-agent-test-transcript-history-user-turn-uses-marker-line ()
  "History replay uses the same marker line and hidden headings as live turns."
  (pi-coding-agent-transcript-test--with-history
      (pi-coding-agent-transcript-test--messages
       '("user" "Hello there")
       '("assistant" "Hi."))
    (let* ((visible (pi-coding-agent-transcript-test--visible))
           (tail (save-match-data
                   (and (string-match
                         (concat "\n" (regexp-quote pi-coding-agent-transcript-test--marker))
                         visible)
                        (substring visible (match-beginning 0))))))
      (should (equal (concat "\n" pi-coding-agent-transcript-test--marker
                             "\nHello there\n\nHi.")
                     (string-trim-right tail)))
      (should-not (string-match-p "^You$" tail))
      (should-not (string-match-p "^Assistant$" tail))
      ;; History headings keep the original timestamp inside hidden structure.
      (should (string-match-p "You \u00b7 2024-01-01 08:00\n=+" (buffer-string)))
      (should-not (string-match-p "2024-01-01" tail)))))

(ert-deftest pi-coding-agent-test-transcript-timestamp-stays-in-help-echo ()
  "The user turn timestamp never enters visible text."
  (pi-coding-agent-transcript-test--with-chat
    (let ((time (encode-time 0 5 10 13 6 2026)))
      (pi-coding-agent--display-user-message "Timed message" time)
      (should-not (string-match-p "2026-06-13" (pi-coding-agent-transcript-test--visible)))
      (let ((marker-pos (save-excursion
                          (goto-char (point-min))
                          (search-forward pi-coding-agent-transcript-test--marker)
                          (1- (point)))))
        (should (string-match-p
                 "2026-06-13 10:05"
                 (or (get-text-property marker-pos 'help-echo) "")))))))

;;;; 1.2 First-line Markdown block starts

(ert-deftest pi-coding-agent-test-transcript-user-first-line-keeps-block-type ()
  "The marker line never changes how the first body line is parsed."
  (dolist (case '(("setext" . "Title\n=====\n\nBody after")
                  ("atx" . "# Heading\n\nBody after")
                  ("fence" . "```python\nprint(1)\n```\n\nBody after")
                  ("table" . "| a | b |\n| --- | --- |\n| 1 | 2 |\n\nBody after")
                  ("list" . "- one\n- two\n\nBody after")
                  ("quote" . "> quoted\n\nBody after")
                  ("indented" . "    indented code\n\nBody after")))
    (pi-coding-agent-transcript-test--with-chat
      (pi-coding-agent--display-user-message (cdr case))
      (pi-coding-agent--display-agent-start)
      (pi-coding-agent--display-message-delta "Answer body")
      (pi-coding-agent--finish-assistant-text)
      (let* ((raw (buffer-string))
             (body-pos (save-excursion (goto-char (point-min))
                                       (search-forward (car (split-string (cdr case) "\n")))
                                       (line-beginning-position))))
        ;; The marker owns its own paragraph; the body starts a fresh block.
        (should (string-match-p
                 (concat (regexp-quote pi-coding-agent-transcript-test--marker)
                         "\n\n")
                 raw))
        (should-not (string-match-p
                     (concat "^"
                             (regexp-quote pi-coding-agent-transcript-test--marker)
                             " ")
                     raw))
        ;; The setext case must not absorb the marker into the heading.
        (when (equal (car case) "setext")
          (should (equal body-pos
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward "Title")
                           (line-beginning-position)))))
        ;; The Assistant body must stay outside the user block.
        (should (string-match-p "Answer body" raw))
        (when-let* ((type (pi-coding-agent-transcript-test--parsed-block-type body-pos)))
          (should (member type
                          '("paragraph" "setext_heading" "atx_heading"
                            "fenced_code_block" "table" "pipe_table"
                            "block_quote" "list" "indented_code_block"))))))))

;;;; 1.3 Multi-line user messages

(ert-deftest pi-coding-agent-test-transcript-multiline-user-message-one-marker ()
  "A multi-line user message keeps exactly one marker line."
  (pi-coding-agent-transcript-test--with-chat
    (pi-coding-agent--display-user-message "first line\nsecond line\nthird line")
    (should (= 1 (how-many (regexp-quote pi-coding-agent-transcript-test--marker))))
    (should (equal (concat "\n" pi-coding-agent-transcript-test--marker
                           "\nfirst line\nsecond line\nthird line\n")
                   (pi-coding-agent-transcript-test--visible)))))

;;;; 1.4 Folded paragraph break between marker and body

(ert-deftest pi-coding-agent-test-transcript-marker-keeps-hidden-paragraph-break ()
  "The marker and body stay adjacent on screen but keep a paragraph break."
  (pi-coding-agent-transcript-test--with-chat
    (pi-coding-agent--display-user-message "Body line")
    (let* ((raw (buffer-string))
           (marker-pos (1- (save-excursion
                             (goto-char (point-min))
                             (search-forward pi-coding-agent-transcript-test--marker)
                             (point)))))
      ;; Two real newlines follow the marker in the buffer.
      (should (equal (buffer-substring-no-properties marker-pos (+ marker-pos 3))
                     (concat pi-coding-agent-transcript-test--marker "\n\n")))
      ;; The second newline is folded, so nothing is drawn for it.
      (should (equal "" (get-text-property (+ marker-pos 2) 'display)))
      ;; Visible result is two adjacent lines.
      (should (equal (concat "\n" pi-coding-agent-transcript-test--marker "\nBody line\n")
                     (pi-coding-agent-transcript-test--visible)))
      ;; Visible copy keeps the marker/newline/body shape.
      (should (equal (concat pi-coding-agent-transcript-test--marker "\nBody line\n")
                     (pi-coding-agent-transcript-test--visible-copy
                      marker-pos (point-max))))
      (should (string-match-p "\n\n" raw)))))

;;;; 1.5 Copy semantics

(ert-deftest pi-coding-agent-test-transcript-visible-copy-omits-structure ()
  "Visible copy drops headings, underline and timestamps but keeps the marker."
  (pi-coding-agent-transcript-test--with-chat
    (let ((time (encode-time 0 5 10 13 6 2026)))
      (pi-coding-agent--display-user-message "Copy me" time)
      (pi-coding-agent--display-agent-start)
      (pi-coding-agent--display-message-delta "Answer")
      (pi-coding-agent--finish-assistant-text)
      (let ((copy (pi-coding-agent-transcript-test--visible-copy
                   (point-min) (point-max))))
        (should (string-match-p pi-coding-agent-transcript-test--marker copy))
        (should (string-match-p "Copy me" copy))
        (should (string-match-p "Answer" copy))
        (should-not (string-match-p "You" copy))
        (should-not (string-match-p "Assistant" copy))
        (should-not (string-match-p "=+" copy))
        (should-not (string-match-p "2026-06-13" copy))))))

(ert-deftest pi-coding-agent-test-transcript-raw-copy-keeps-structure ()
  "Raw Markdown copy still returns the structural headings."
  (pi-coding-agent-transcript-test--with-chat
    (let ((pi-coding-agent-copy-raw-markdown t))
      (pi-coding-agent--display-user-message "Raw copy")
      (let ((copy (pi-coding-agent-transcript-test--visible-copy
                   (point-min) (point-max))))
        (should (string-match-p "You\n===" copy))
        (should (string-match-p pi-coding-agent-transcript-test--marker copy))))))

;;;; 1.6 isearch does not reveal hidden headings

(ert-deftest pi-coding-agent-test-transcript-search-invisible-is-nil ()
  "Chat buffers force `search-invisible' to nil."
  (pi-coding-agent-transcript-test--with-chat
    (should (local-variable-p 'search-invisible))
    (should-not search-invisible)))

(ert-deftest pi-coding-agent-test-transcript-isearch-skips-hidden-heading ()
  "Incremental search does not stop on the hidden `You' heading."
  (pi-coding-agent-transcript-test--with-chat
    (pi-coding-agent--display-user-message "Search body")
    ;; `isearch-range-invisible' is the predicate isearch applies with the
    ;; buffer's `search-invisible' value.
    (let ((heading (pi-coding-agent-transcript-test--heading-pos "You"))
          (body (save-excursion
                  (goto-char (point-min))
                  (search-forward "Search body")
                  (line-beginning-position))))
      (should (isearch-range-invisible heading (+ heading 3)))
      (should-not (isearch-range-invisible body (+ body 6))))))

(ert-deftest pi-coding-agent-test-transcript-search-forward-sees-structure ()
  "Programmatic search still reaches the structural text (documented limit)."
  (pi-coding-agent-transcript-test--with-chat
    (pi-coding-agent--display-user-message "Body")
    (goto-char (point-min))
    (should (search-forward "You" nil t))))

;;;; 1.7 Successful tool summaries

(ert-deftest pi-coding-agent-test-transcript-bash-summary-is-one-line ()
  "A multi-line bash result shows its header plus one summary line."
  (let ((output (concat (mapconcat (lambda (n) (format "line-%d" n))
                                   (number-sequence 1 20)
                                   "\n")
                        "\nTAIL_MARKER")))
    (pi-coding-agent-transcript-test--with-history
        (pi-coding-agent-transcript-test--tool-messages
         "bash" '(:command "run-tests") output)
      (let ((raw (buffer-string))
            (visible (pi-coding-agent-transcript-test--visible)))
        (should (string-match-p "\\$ run-tests" raw))
        (should (string-match-p "21 lines \u00b7 line-1" raw))
        (should-not (string-match-p "TAIL_MARKER" visible))
        (should-not (string-match-p "hidden" visible))
        (should-not (string-match-p "TAB details" visible))
        (should-not (string-search "[details]" visible))
        (should-not (string-match-p "line-20" visible))))))

(ert-deftest pi-coding-agent-test-transcript-generic-header-stays-multiline ()
  "Long generic arguments keep the existing pretty multi-line header."
  (let ((fill-column 40))
    (pi-coding-agent-transcript-test--with-history
        (pi-coding-agent-transcript-test--tool-messages
         "subagent" '(:agent "worker" :task "Search for weather") "done")
      (let* ((ov (seq-find (lambda (candidate)
                             (overlay-get candidate 'pi-coding-agent-tool-block))
                           (overlays-in (point-min) (point-max))))
             (header-end (and ov (marker-position
                                  (overlay-get ov 'pi-coding-agent-header-end))))
             (header (and ov (string-trim-right
                              (buffer-substring-no-properties
                               (overlay-start ov) header-end))))
             (body (and ov (string-trim-right
                            (buffer-substring-no-properties
                             header-end (overlay-end ov))))))
        (should ov)
        ;; Header keeps its pretty multi-line form.
        (should (< 1 (length (split-string header "\n"))))
        ;; The result summary after that header is exactly one line.
        (should (= 1 (length (split-string body "\n"))))
        (should (string-match-p "fields" body))))))

;;;; 1.8 Failed tool summaries

(ert-deftest pi-coding-agent-test-transcript-error-summary-single-line ()
  "A multi-line error collapses to one line and TAB restores the detail."
  (let ((error-text "FIRST_LINE_ERROR\nsecond line\nthird line"))
    (pi-coding-agent-transcript-test--with-history
        (pi-coding-agent-transcript-test--tool-messages
         "bash" '(:command "false") error-text nil t)
      (let ((raw (buffer-string)))
        (should (string-match-p "failed \u00b7 FIRST_LINE_ERROR" raw))
        (should-not (string-match-p "second line" raw))
        (should (= 1 (save-excursion
                       (goto-char (point-min))
                       (how-many "failed \u00b7")))))
      (let ((button (pi-coding-agent-transcript-test--summary-button)))
        (should button)
        (pi-coding-agent--toggle-tool-summary button)
        (should (string-match-p "second line" (buffer-string)))))))

(ert-deftest pi-coding-agent-test-transcript-error-summary-uses-bounded-prefix ()
  "The summary only inspects the bounded output prefix."
  (let* ((limit pi-coding-agent--tool-summary-error-bytes)
         (payload (concat (make-string (1+ limit) ?\n) "LATE_LINE")))
    (cl-letf (((symbol-function 'pi-coding-agent--tool-summary-tail)
               (lambda (&rest _) (ert-fail "tail helper must not be used"))))
      (let ((record (pi-coding-agent--tool-summary-record
                     "call-x" "bash" '(:command "false")
                     (vector (list :type "text" :text payload)) nil t)))
        (should (equal "failed \u00b7 2050 lines" (plist-get record :summary)))))))

;;;; 1.9 Summary text cannot break Markdown

(ert-deftest pi-coding-agent-test-transcript-summary-fence-fragment-is-safe ()
  "A fence-like or table-like summary line cannot swallow later Markdown."
  (let ((error-text "```js | a | b |"))
    (pi-coding-agent-transcript-test--with-history
        (pi-coding-agent-transcript-test--tool-messages
         "bash" '(:command "false") error-text nil t)
      (should-not (string-match-p "^```" (pi-coding-agent-transcript-test--visible)))
      (should (string-match-p
               (regexp-quote "```js | a | b |")
               (substring-no-properties (buffer-string)))))))

;;;; 1.10 Thinking stub

(ert-deftest pi-coding-agent-test-transcript-thinking-stub-previews-first-line ()
  "Collapsed thinking previews its first non-empty line."
  (should (equal "▸ Think · first line"
                 (pi-coding-agent--thinking-hidden-stub "\n\n  first line\nsecond")))
  (should (equal "▸ Think · only"
                 (pi-coding-agent--thinking-hidden-stub "only")))
  (should (equal "▸ Think"
                 (pi-coding-agent--thinking-hidden-stub "\n\n"))))

(ert-deftest pi-coding-agent-test-transcript-thinking-stub-obeys-column-budget ()
  "Long and wide first lines are truncated to the column budget."
  (dolist (text (list (make-string 200 ?x)
                      (make-string 200 ?漢)))
    (let* ((stub (pi-coding-agent--thinking-hidden-stub text))
           (preview (string-trim (substring stub (length "▸ Think · ")))))
      (should (string-suffix-p "\u2026" preview))
      (should (<= (string-width preview)
                  (- pi-coding-agent--single-line-preview-columns
                     (string-width "▸ Think · ")))))))

(ert-deftest pi-coding-agent-test-transcript-thinking-stub-ignores-obsolete-option ()
  "The obsolete preview option no longer changes the stub."
  (let ((pi-coding-agent-thinking-hidden-preview nil))
    (should (equal "▸ Think · keep previewing"
                   (pi-coding-agent--thinking-hidden-stub "keep previewing")))))

;;;; 1.11 Whole-line summary button

(ert-deftest pi-coding-agent-test-transcript-summary-line-is-a-button ()
  "The whole summary line is a toggle button."
  (pi-coding-agent-transcript-test--with-history
      (pi-coding-agent-transcript-test--tool-messages
       "bash" '(:command "ls") "one\ntwo")
    (goto-char (point-min))
    (search-forward "2 lines")
    (let* ((button (button-at (1- (point))))
           (line-start (line-beginning-position))
           (line-end (line-end-position)))
      (should button)
      (should (button-get button 'pi-coding-agent-tool-toggle))
      (should (equal "call-1" (button-get button 'pi-coding-agent-tool-call-id)))
      (should (equal "2 lines \u00b7 one" (button-get button 'pi-coding-agent-summary)))
      ;; Button covers the whole line, including its trailing newline.
      (should (button-at line-start))
      (should (button-at line-end))
      (should (string-match-p "TAB" (or (button-get button 'help-echo) "")))
      ;; No button face: the line must stay visually quiet.
      (should-not (get-text-property line-start 'face)))))

(ert-deftest pi-coding-agent-test-transcript-tab-anywhere-in-summary-row-toggles ()
  "TAB on the header row or anywhere in the summary row toggles the detail."
  (dolist (probe '("header" "summary-start" "summary-middle" "summary-end"))
    (pi-coding-agent-transcript-test--with-history
        (pi-coding-agent-transcript-test--tool-messages
         "bash" '(:command "ls") "one\ntwo")
      (goto-char (point-min))
      (search-forward "2 lines")
      (let ((summary-start (line-beginning-position)))
        (goto-char (pcase probe
                     ("header" (save-excursion
                                 (goto-char (point-min))
                                 (search-forward "$ ls")
                                 (line-beginning-position)))
                     ("summary-start" summary-start)
                     ("summary-middle" (+ summary-start 3))
                     ("summary-end" (1- (line-end-position)))))
        (pi-coding-agent-toggle-tool-section)
        (should (string-match-p "one\ntwo\n" (buffer-string)))
        ;; A second TAB collapses back to the one-line summary.
        (let ((btn (pi-coding-agent-transcript-test--summary-button)))
          (should btn)
          (pi-coding-agent--toggle-tool-summary btn)
          (should-not (string-match-p "one\ntwo\n" (buffer-string)))
          (should (string-match-p "2 lines \u00b7 one" (buffer-string))))))))

(ert-deftest pi-coding-agent-test-transcript-ret-expands-summary-and-keeps-file-nav ()
  "RET expands on the summary row and still visits files on the header row."
  (pi-coding-agent-transcript-test--with-history
      (pi-coding-agent-transcript-test--tool-messages
       "read" '(:path "/tmp/pi-transcript/target.el") "line1\nline2")
    (goto-char (point-min))
    (search-forward "2 lines")
    (let ((summary-pos (line-beginning-position)))
      (goto-char summary-pos)
      ;; The summary line is a button, so RET dispatches the button action
      ;; (`pi-coding-agent--dispatch-button' keeps tool toggles as-is) rather
      ;; than the file visitor.
      (should (button-at summary-pos))
      (pi-coding-agent--dispatch-button summary-pos nil t)
      (should (string-match-p "line1\nline2" (buffer-string)))
      (goto-char (point-min))
      (search-forward "read /tmp/pi-transcript/target.el")
      ;; The header row has no toggle button, so RET stays a file visit.
      (should-not (button-at (line-beginning-position)))
      (should (equal "/tmp/pi-transcript/target.el"
                     (plist-get (pi-coding-agent--file-target-at-point)
                                :emacs-path))))))

;;;; 1.12 Cooling and resume keep the one-line form

(ert-deftest pi-coding-agent-test-transcript-cooled-summary-is-one-line ()
  "Cooled and resumed tool blocks keep header plus one summary line."
  (let ((pi-coding-agent-hot-tail-turn-count 0))
    (pi-coding-agent-transcript-test--with-history
        (pi-coding-agent-transcript-test--tool-messages
         "bash" '(:command "run-tests") "alpha\nbeta\nTAIL_KEEP")
      (let ((raw (buffer-string)))
        (should (string-match-p "3 lines \u00b7 alpha" raw))
        (should-not (string-match-p "TAIL_KEEP" raw))
        (should-not (string-match-p "hidden" raw))
        (should-not (string-match-p "TAB details" raw))
        (should-not (pi-coding-agent-transcript-test--summary-button)))
      ;; A rebuild reproduces the same cold form.
      (let ((before (buffer-string)))
        (pi-coding-agent--rerender-canonical-history)
        (should (equal before (buffer-string)))))))

;;;; 1.13 Navigation, fork and outline keep working

(ert-deftest pi-coding-agent-test-transcript-navigation-uses-hidden-headings ()
  "Turn navigation, fork ordinals and hot-tail boundaries still see headings."
  (pi-coding-agent-transcript-test--with-history
      (pi-coding-agent-transcript-test--messages
       '("user" "first question")
       '("assistant" "first answer")
       '("user" "second question")
       '("assistant" "second answer"))
    (let ((headings (pi-coding-agent--collect-you-headings)))
      (should (= 2 (length headings)))
      (goto-char (point-max))
      (should (= 1 (pi-coding-agent--user-turn-index-at-point headings)))
      (goto-char (car headings))
      (should (pi-coding-agent--at-you-heading-p))
      (should (= 0 (pi-coding-agent--user-turn-index-at-point headings)))
      ;; Hot-tail boundary lands on a turn heading.
      (let ((pi-coding-agent-hot-tail-turn-count 1))
        (pi-coding-agent--update-hot-tail-boundary)
        (goto-char (marker-position pi-coding-agent--hot-tail-start))
        (should (pi-coding-agent--at-turn-heading-p))))))

;;;; 1.14 Refontification keeps headings hidden

(ert-deftest pi-coding-agent-test-transcript-refontify-keeps-headings-hidden ()
  "`font-lock-flush' + `font-lock-ensure' must not reveal hidden headings."
  (pi-coding-agent-transcript-test--with-chat
    (pi-coding-agent--display-user-message "first")
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "answer")
    (let ((before (pi-coding-agent-transcript-test--visible)))
      (font-lock-flush)
      (font-lock-ensure)
      ;; `font-lock-ensure' is a no-op without `font-lock-mode' (as in batch
      ;; temp buffers), so drive the jit-lock cycle redisplay performs.
      (jit-lock-fontify-now (point-min) (point-max))
      (should (pi-coding-agent-transcript-test--hidden-structure-at-p
               (pi-coding-agent-transcript-test--heading-pos "You")))
      (should (equal before (pi-coding-agent-transcript-test--visible)))
      (should (equal (concat "\n" pi-coding-agent-transcript-test--marker
                             "\nfirst\n\nanswer")
                     (string-trim-right
                      (pi-coding-agent-transcript-test--visible)))))))

;;;; 1.15 Column budget and allocation discipline

(ert-deftest pi-coding-agent-test-transcript-summary-budget-and-scan ()
  "Summaries fit the column budget and never format the full payload."
  (let ((payload (mapconcat (lambda (n)
                              (concat (format "row-%d " n)
                                      (make-string 200 ?y)))
                            (number-sequence 1 1500)
                            "\n")))
    (cl-letf (((symbol-function 'ansi-color-filter-apply)
               (lambda (&rest _) (ert-fail "ANSI formatter called")))
              ((symbol-function 'pi-coding-agent--pretty-print-json)
               (lambda (&rest _) (ert-fail "JSON formatter called")))
              ((symbol-function 'pi-coding-agent--insert-rendered-tool-content)
               (lambda (&rest _) (ert-fail "Fence renderer called")))
              ((symbol-function 'pi-coding-agent--apply-diff-overlays)
               (lambda (&rest _) (ert-fail "Diff renderer called"))))
      (dolist (record (list
                       (pi-coding-agent--tool-summary-record
                        "call-1" "bash" '(:command "big")
                        (vector (list :type "text" :text payload)) nil nil)
                       (pi-coding-agent--tool-summary-record
                        "call-2" "bash" '(:command "big")
                        (vector (list :type "text" :text payload)) nil t)))
        (let ((summary (plist-get record :summary)))
          (should (stringp summary))
          (should-not (string-match-p "\n" summary))
          (should (<= (string-width summary)
                      pi-coding-agent--single-line-preview-columns))
          (should-not (string-match-p (make-string 120 ?y) summary)))))))

(provide 'pi-coding-agent-transcript-test)
;;; pi-coding-agent-transcript-test.el ends here
