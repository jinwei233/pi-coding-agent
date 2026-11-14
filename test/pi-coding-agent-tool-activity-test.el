;;; pi-coding-agent-tool-activity-test.el --- Grouped tool activity -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract tests for one lightweight tool activity group per user turn.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)
(require 'pi-coding-agent-progress-fixture)

(defmacro pi-coding-agent-tool-activity-test--with-chat (&rest body)
  "Run BODY in a fresh grouped-activity chat buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (pi-coding-agent-chat-mode)
     (pi-coding-agent--set-chat-session-identity "/tmp/pi-tool-activity/")
     (setq-local pi-coding-agent--tool-activity-display 'grouped)
     ,@body))

(defun pi-coding-agent-tool-activity-test--visible ()
  "Return plain visible text of the current buffer."
  (substring-no-properties
   (pi-coding-agent--visible-text (point-min) (point-max))))

(defun pi-coding-agent-tool-activity-test--render-tool
    (id name args content &optional details error)
  "Render one completed tool."
  (pi-coding-agent--handle-display-event
   (list :type "tool_execution_start"
         :toolCallId id :toolName name :args args))
  (pi-coding-agent--handle-display-event
   (list :type "tool_execution_end"
         :toolCallId id :toolName name
         :result (list :content
                       (vector (list :type "text" :text content))
                       :details details)
         :isError error)))

(defun pi-coding-agent-tool-activity-test--messages (&rest turns)
  "Return canonical history built from TURNS.
Each turn is a list of tool specs (NAME ARGS CONTENT DETAILS ERROR)."
  (let ((messages nil)
        (id 0))
    (dolist (turn turns)
      (push (list :role "user"
                  :content (format "turn %d" (1+ id))
                  :timestamp 1704067200000)
            messages)
      (dolist (spec turn)
        (setq id (1+ id))
        (let ((call-id (format "call-%d" id))
              (name (nth 0 spec))
              (args (nth 1 spec))
              (content (nth 2 spec))
              (details (nth 3 spec))
              (error (nth 4 spec)))
          (push (list :role "assistant"
                      :content
                      (vector
                       (list :type "toolCall" :id call-id
                             :name name :arguments args)))
                messages)
          (push (list :role "toolResult"
                      :toolCallId call-id
                      :toolName name
                      :content
                      (vector (list :type "text" :text content))
                      :details details
                      :isError (and error t))
                messages))))
    (vconcat (nreverse messages))))

(defun pi-coding-agent-tool-activity-test--render-history (&rest turns)
  "Render grouped canonical history from TURNS."
  (pi-coding-agent--display-session-history
   (apply #'pi-coding-agent-tool-activity-test--messages turns)
   (current-buffer)))

(defun pi-coding-agent-tool-activity-test--goto (text)
  "Move point to the start of TEXT."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(defun pi-coding-agent-tool-activity-test--goto-group ()
  "Move point to the current activity group by structural metadata."
  (goto-char (point-min))
  (let ((position
         (text-property-any
          (point-min) (point-max)
          'pi-coding-agent-activity-group t)))
    (unless position
      (ert-fail "No grouped activity row found"))
    (goto-char position)))

(defun pi-coding-agent-tool-activity-test--count (regexp)
  "Count visible matches of REGEXP."
  (let ((text (pi-coding-agent-tool-activity-test--visible))
        (start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(ert-deftest pi-coding-agent-test-tool-activity-live-groups-one-turn ()
  "Live successes produce one result-first group and no tool rows."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run")
    (pi-coding-agent-tool-activity-test--render-tool
     "c1" "read" '(:path "/tmp/pi-tool-activity/a.el") "secret")
    (pi-coding-agent-tool-activity-test--render-tool
     "c2" "edit" '(:path "/tmp/pi-tool-activity/a.el") "done"
     '(:diff "+ 1 new\n- 1 old"))
    (pi-coding-agent--handle-display-event '(:type "agent_end"))
    (let ((visible (pi-coding-agent-tool-activity-test--visible)))
      (should (= 1 (pi-coding-agent-tool-activity-test--count "Tools")))
      (should (string-match-p "edited 1" visible))
      (should (string-match-p "+1 -1" visible))
      (should (string-match-p "2 calls" visible))
      (should-not (string-match-p "secret" visible))
      (should-not (string-match-p "\n    read" visible)))))

(ert-deftest pi-coding-agent-test-tool-activity-history-matches-live-shape ()
  "History directly builds the same one-group projection."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("read" (:path "/tmp/pi-tool-activity/a.el") "secret")
       ("bash" (:command "make test") "ok")))
    (should (= 1 (pi-coding-agent-tool-activity-test--count "Tools")))
    (should (string-match-p "2 calls"
                            (pi-coding-agent-tool-activity-test--visible)))
    (should-not
     (seq-some (lambda (overlay)
                 (overlay-get overlay 'pi-coding-agent-tool-block))
               (overlays-in (point-min) (point-max))))
    (should (= 1 pi-coding-agent--history-activity-initializations))))

(ert-deftest pi-coding-agent-test-tool-activity-one-group-per-turn ()
  "Each user turn owns at most one activity group."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "one") "ok")
       ("bash" (:command "two") "ok"))
     '(("read" (:path "/tmp/pi-tool-activity/b.el") "ok")))
    (should (= 2 (pi-coding-agent-tool-activity-test--count "Tools")))
    (should (= 2 (hash-table-count pi-coding-agent--activity-table)))))

(ert-deftest pi-coding-agent-test-tool-activity-no-empty-group ()
  "A turn without tools has no activity group."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-session-history
     (vector '(:role "user" :content "plain")
             '(:role "assistant"
               :content [(:type "text" :text "answer")]))
     (current-buffer))
    (should-not (string-match-p
                 "Tools" (pi-coding-agent-tool-activity-test--visible)))))

(ert-deftest pi-coding-agent-test-tool-activity-live-folded-row-is-stable ()
  "A folded live row stays unchanged until the agent turn settles."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run")
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "c1"
       :toolName "bash" :args (:command "make test")))
    (let ((live-row (pi-coding-agent-tool-activity-test--visible)))
      (should (string-match-p "Tools · working" live-row))
      (should (= 1 (pi-coding-agent-tool-activity-test--count "Tools")))
      (pi-coding-agent--handle-display-event
       '(:type "tool_execution_update" :toolCallId "c1"
         :partialResult (:content [(:type "text" :text "half")])))
      (pi-coding-agent--handle-display-event
       '(:type "tool_execution_end" :toolCallId "c1" :toolName "bash"
         :result (:content [(:type "text" :text "done")])
         :isError nil))
      (pi-coding-agent--handle-display-event
       '(:type "tool_execution_start" :toolCallId "c2"
         :toolName "read" :args (:path "/tmp/pi-tool-activity/a.el")))
      (pi-coding-agent--handle-display-event
       '(:type "tool_execution_end" :toolCallId "c2" :toolName "read"
         :result (:content [(:type "text" :text "contents")])
         :isError nil))
      (should (equal live-row
                     (pi-coding-agent-tool-activity-test--visible))))
    (pi-coding-agent--handle-display-event '(:type "agent_end"))
    (let ((settled (pi-coding-agent-tool-activity-test--visible)))
      (should-not (string-match-p "working" settled))
      (should (string-match-p "read 1" settled))
      (should (string-match-p "2 calls" settled)))))

(ert-deftest pi-coding-agent-test-tool-activity-failure-has-priority ()
  "Failures remain visible in the folded group."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "false") "boom\nprivate tail" nil t)
       ("read" (:path "/tmp/pi-tool-activity/a.el") "ok")))
    (let ((visible (pi-coding-agent-tool-activity-test--visible)))
      (should (string-match-p "1 call failed" visible))
      (should-not (string-match-p "private tail" visible)))))

(ert-deftest pi-coding-agent-test-tool-activity-structured-test-status ()
  "Only structured test status contributes a test result."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("custom" (:query "tests") "opaque"
        (:testStatus "passed"))))
    (should (string-match-p
             "tests passed" (pi-coding-agent-tool-activity-test--visible)))))

(ert-deftest pi-coding-agent-test-tool-activity-group-tab-expands-local-tools ()
  "TAB on the group reveals only its tool rows."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "make test") "alpha")
       ("read" (:path "/tmp/pi-tool-activity/a.el") "beta")))
    (pi-coding-agent-tool-activity-test--goto-group)
    (pi-coding-agent-toggle-tool-section)
    (let ((visible (pi-coding-agent-tool-activity-test--visible)))
      (should (string-match-p "▾ Tools" visible))
      (should (string-match-p "\n    bash" visible))
      (should (string-match-p "\n    read" visible)))
    (pi-coding-agent-tool-activity-test--goto-group)
    (pi-coding-agent-toggle-tool-section)
    (should-not (string-match-p
                 "\n    bash" (pi-coding-agent-tool-activity-test--visible)))))

(ert-deftest pi-coding-agent-test-tool-activity-tool-tab-toggles-detail ()
  "TAB on a tool row toggles its inline detail."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "make test") "alpha\nbeta")))
    (pi-coding-agent-tool-activity-test--goto-group)
    (pi-coding-agent-toggle-tool-section)
    (pi-coding-agent-tool-activity-test--goto "    bash")
    (pi-coding-agent-toggle-tool-section)
    (should (string-match-p
             "    alpha\n    beta"
             (buffer-substring-no-properties (point-min) (point-max))))
    (pi-coding-agent-tool-activity-test--goto "    alpha")
    (pi-coding-agent-toggle-tool-section)
    (should-not (string-match-p
                 "    alpha"
                 (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest pi-coding-agent-test-tool-activity-body-tab-uses-outline ()
  "TAB in ordinary transcript text keeps outline semantics."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "make test") "ok")))
    (let ((called nil))
      (cl-letf (((symbol-function 'outline-cycle)
                 (lambda (&rest _) (setq called t))))
        (pi-coding-agent-tool-activity-test--goto "turn 1")
        (pi-coding-agent-toggle-tool-section)
        (should called)
        (should-not (string-match-p
                     "\n    bash"
                     (pi-coding-agent-tool-activity-test--visible)))))))

(ert-deftest pi-coding-agent-test-tool-activity-ret-actions ()
  "RET toggles a group and uses metadata for file rows."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("read" (:path "/tmp/pi-tool-activity/a.el" :offset 7) "line")))
    (pi-coding-agent-tool-activity-test--goto-group)
    (pi-coding-agent--dispatch-button (point) nil t)
    (should (string-match-p
             "\n    read" (pi-coding-agent-tool-activity-test--visible)))
    (let (target)
      (cl-letf (((symbol-function 'pi-coding-agent--visit-file-target)
                 (lambda (value _toggle) (setq target value))))
        (pi-coding-agent-tool-activity-test--goto "    read")
        (pi-coding-agent--dispatch-button (point) nil t))
      (should (equal "/tmp/pi-tool-activity/a.el"
                     (plist-get target :emacs-path)))
      (should (= 7 (plist-get target :line))))))

(ert-deftest pi-coding-agent-test-tool-activity-preserves-user-expansion ()
  "Later tool completion does not collapse an expanded group."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run")
    (pi-coding-agent-tool-activity-test--render-tool
     "c1" "bash" '(:command "one") "ok")
    (pi-coding-agent-tool-activity-test--goto-group)
    (pi-coding-agent-toggle-tool-section)
    (pi-coding-agent-tool-activity-test--render-tool
     "c2" "bash" '(:command "two") "ok")
    (let ((visible (pi-coding-agent-tool-activity-test--visible)))
      (should (string-match-p "▾ Tools" visible))
      (should (= 2 (pi-coding-agent-tool-activity-test--count
                    "\n    bash"))))))

(ert-deftest pi-coding-agent-test-tool-activity-record-is-lightweight ()
  "Activity records exclude complete output, diff, and write payloads."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run")
    (pi-coding-agent-tool-activity-test--render-tool
     "c1" "edit" '(:path "/tmp/pi-tool-activity/a.el")
     "SECRET_OUTPUT" '(:diff "+ SECRET_DIFF"))
    (let* ((record (gethash 0 pi-coding-agent--activity-table))
           (tool (car (pi-coding-agent--activity-record-tools record)))
           (printed (prin1-to-string tool)))
      (should-not (string-match-p "SECRET_OUTPUT" printed))
      (should-not (string-match-p "SECRET_DIFF" printed))
      (should (equal '(:tool-call-id "c1")
                     (plist-get tool :detail-locator))))))

(ert-deftest pi-coding-agent-test-tool-activity-folded-copy-excludes-payload ()
  "Visible and raw transcript copies contain no unmaterialized payload."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "make test") "SECRET_OUTPUT")))
    (dolist (raw '(nil t))
      (let ((pi-coding-agent-copy-raw-markdown raw)
            (copy (filter-buffer-substring (point-min) (point-max))))
        (should (string-match-p "Tools" copy))
        (should-not (string-match-p "SECRET_OUTPUT" copy))))))

(ert-deftest pi-coding-agent-test-tool-activity-summary-is-column-bounded ()
  "A grouped summary stays within its display-column budget."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("edit"
        (:path "/tmp/pi-tool-activity/a-very-long-directory/a.el")
        "done" (:diff "+ new\n- old") nil)
       ("bash" (:command "test") "boom" nil t)))
    (pi-coding-agent-tool-activity-test--goto-group)
    (should (<= (string-width
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))
                pi-coding-agent--single-line-preview-columns))))

(ert-deftest pi-coding-agent-test-tool-activity-update-does-not-scan-history ()
  "Live completion and one-turn expansion avoid global scans."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run")
    (let ((headings 0)
          (overlays 0))
      (cl-letf (((symbol-function 'pi-coding-agent--collect-you-headings)
                 (lambda (&rest _) (setq headings (1+ headings)) nil))
                ((symbol-function 'overlays-in)
                 (lambda (&rest _) (setq overlays (1+ overlays)) nil)))
        (pi-coding-agent-tool-activity-test--render-tool
         "c1" "bash" '(:command "make test") "ok")
        (pi-coding-agent-tool-activity-test--goto-group)
        (pi-coding-agent-toggle-tool-section))
      (should (= 0 headings))
      (should (= 0 overlays)))))

(ert-deftest pi-coding-agent-test-tool-activity-history-initializes-once ()
  "A replay initializes once and renders each turn only once."
  (pi-coding-agent-tool-activity-test--with-chat
    (let ((initialize
           (symbol-function 'pi-coding-agent--initialize-history-activity))
          (render (symbol-function 'pi-coding-agent--activity-render-record))
          (initializations 0)
          (renders 0))
      (cl-letf (((symbol-function 'pi-coding-agent--initialize-history-activity)
                 (lambda ()
                   (setq initializations (1+ initializations))
                   (funcall initialize)))
                ((symbol-function 'pi-coding-agent--activity-render-record)
                 (lambda (record)
                   (setq renders (1+ renders))
                   (funcall render record))))
        (pi-coding-agent-tool-activity-test--render-history
         '(("bash" (:command "one") "ok")
           ("bash" (:command "two") "ok"))
         '(("bash" (:command "three") "ok"))))
      (should (= 1 initializations))
      (should (= 1 pi-coding-agent--history-activity-initializations))
      (should (= 2 renders)))))

(ert-deftest pi-coding-agent-test-tool-activity-summary-fallback ()
  "Summary mode keeps the established per-tool presentation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--set-chat-session-identity "/tmp/pi-tool-activity/")
    (setq-local pi-coding-agent--tool-activity-display 'summary)
    (pi-coding-agent-tool-activity-test--render-history
     '(("read" (:path "/tmp/pi-tool-activity/a.el") "line")))
    (let ((visible (pi-coding-agent-tool-activity-test--visible)))
      (should-not (string-match-p "Tools" visible))
      (should (string-match-p "read /tmp/pi-tool-activity/a.el" visible)))))

(ert-deftest pi-coding-agent-test-tool-activity-hidden-aliases-grouped ()
  "The old hidden value maps to grouped for one compatibility cycle."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--tool-activity-display 'hidden)
    (should (eq 'grouped (pi-coding-agent--tool-activity-mode)))
    (should-not (pi-coding-agent--tool-activity-hidden-mode-p))
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "one") "ok")))
    (let ((headings 0)
          (property-scans 0))
      (cl-letf (((symbol-function 'pi-coding-agent--collect-you-headings)
                 (lambda (&rest _) (setq headings (1+ headings)) nil))
                ((symbol-function 'next-single-char-property-change)
                 (lambda (&rest _)
                   (setq property-scans (1+ property-scans))
                   (point-max)))
                ((symbol-function 'outline-cycle) #'ignore))
        (pi-coding-agent-tool-activity-test--goto "turn 1")
        (pi-coding-agent-toggle-tool-section))
      (should (= 0 headings))
      (should (= 0 property-scans)))))

(ert-deftest pi-coding-agent-test-tool-activity-completed-path-is-normalized ()
  "Completed grouped records retain normalized and raw path metadata."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("read" (:path "lisp/a.el" :offset 3) "line")))
    (let* ((record (gethash 0 pi-coding-agent--activity-table))
           (tool (car (pi-coding-agent--activity-record-tools record))))
      (should (equal "/tmp/pi-tool-activity/lisp/a.el"
                     (plist-get tool :path)))
      (should (equal "lisp/a.el" (plist-get tool :raw-path))))))

(ert-deftest pi-coding-agent-test-tool-activity-nonfile-ret-opens-detail ()
  "RET on a non-file tool row opens complete detail."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "make test") "ok")))
    (pi-coding-agent-tool-activity-test--goto-group)
    (pi-coding-agent--dispatch-button (point) nil t)
    (let (opened)
      (cl-letf (((symbol-function 'pi-coding-agent--open-tool-detail-buffer)
                 (lambda (tool-call-id) (setq opened tool-call-id))))
        (pi-coding-agent-tool-activity-test--goto "    bash")
        (pi-coding-agent--dispatch-button (point) nil t))
      (should (equal "call-1" opened)))))

(ert-deftest pi-coding-agent-test-tool-activity-inline-detail-navigation ()
  "Expanded read detail rows retain exact physical line navigation."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("read" (:path "lisp/a.el" :offset 7) "alpha\nbeta")))
    (pi-coding-agent-tool-activity-test--goto-group)
    (pi-coding-agent-toggle-tool-section)
    (pi-coding-agent-tool-activity-test--goto "    read")
    (pi-coding-agent-toggle-tool-section)
    (pi-coding-agent-tool-activity-test--goto "    beta")
    (let ((target (pi-coding-agent--file-target-at-point)))
      (should (equal "/tmp/pi-tool-activity/lisp/a.el"
                     (plist-get target :emacs-path)))
      (should (= 8 (plist-get target :line))))))

(ert-deftest pi-coding-agent-test-tool-activity-external-detail-navigation ()
  "Large detail buffers retain exact file navigation metadata."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("write" (:path "lisp/a.el" :content "one\ntwo") "done")))
    ;; A preceding menu test may leave its transient keymap installed.
    ;; This contract concerns the detail buffer's own RET binding.
    (let ((overriding-terminal-local-map nil)
          (overriding-local-map nil)
          detail-buffer)
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buffer &rest _) (setq detail-buffer buffer))))
        (pi-coding-agent--open-tool-detail-buffer "call-1"))
      (unwind-protect
          (with-current-buffer detail-buffer
            (should (eq #'pi-coding-agent-visit-file
                        (key-binding (kbd "RET"))))
            (goto-char (point-min))
            (forward-line 1)
            (let ((target (pi-coding-agent--file-target-at-point)))
              (should (equal "/tmp/pi-tool-activity/lisp/a.el"
                             (plist-get target :emacs-path)))
              (should (= 2 (plist-get target :line)))))
        (when (buffer-live-p detail-buffer)
          (kill-buffer detail-buffer))))))

(ert-deftest pi-coding-agent-test-tool-activity-history-multiple-calls-one-message ()
  "Multiple toolCall blocks in one assistant message preserve source order."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-session-history
     (vector
      '(:role "user" :content "run")
      '(:role "assistant"
        :content [(:type "toolCall" :id "c1" :name "bash"
                   :arguments (:command "one"))
                  (:type "toolCall" :id "c2" :name "bash"
                   :arguments (:command "two"))])
      '(:role "toolResult" :toolCallId "c1" :toolName "bash"
        :content [(:type "text" :text "one")] :isError nil)
      '(:role "toolResult" :toolCallId "c2" :toolName "bash"
        :content [(:type "text" :text "two")] :isError nil))
     (current-buffer))
    (let* ((record (gethash 0 pi-coding-agent--activity-table))
           (ids (mapcar
                 (lambda (tool) (plist-get tool :tool-call-id))
                 (pi-coding-agent--activity-record-tools record))))
      (should (equal '("c1" "c2") ids)))))

(ert-deftest pi-coding-agent-test-tool-activity-retry-restores-working ()
  "A retry after agent end immediately restores the stable working row."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run")
    (pi-coding-agent-tool-activity-test--render-tool
     "c1" "bash" '(:command "one") "ok")
    (pi-coding-agent--handle-display-event '(:type "agent_end"))
    (should-not (string-match-p
                 "working" (pi-coding-agent-tool-activity-test--visible)))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "c2"
       :toolName "bash" :args (:command "two")))
    (should (string-match-p
             "Tools · working"
             (pi-coding-agent-tool-activity-test--visible)))))

(ert-deftest pi-coding-agent-test-tool-activity-process-exit-settles-working ()
  "An interrupted live turn converges to a failed final summary."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run")
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "c1"
       :toolName "bash" :args (:command "one")))
    (pi-coding-agent--activity-interrupt-live-turn)
    (let ((visible (pi-coding-agent-tool-activity-test--visible)))
      (should-not (string-match-p "working" visible))
      (should (string-match-p "1 call failed" visible)))))

(ert-deftest pi-coding-agent-test-tool-activity-priority-and-tiny-budget ()
  "Summary omission is priority ordered and never exceeds tiny windows."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent--display-user-message "run")
    (pi-coding-agent-tool-activity-test--render-tool
     "c1" "edit" '(:path "a.el") "done"
     '(:diff "+ 1 new" :testStatus "failed"))
    (pi-coding-agent--activity-finalize-live-turn)
    (let ((record (gethash 0 pi-coding-agent--activity-table)))
      (cl-letf (((symbol-function 'pi-coding-agent--activity-line-budget)
                 (lambda () 22)))
        (let ((text (substring-no-properties
                     (pi-coding-agent--activity-group-text record))))
          (should (string-match-p "tests failed" text))
          (should-not (string-match-p "calls" text))))
      (cl-letf (((symbol-function 'pi-coding-agent--activity-line-budget)
                 (lambda () 4)))
        (should (<= (string-width
                     (pi-coding-agent--activity-group-text record))
                    4))))))

(ert-deftest pi-coding-agent-test-tool-activity-clear-releases-markers ()
  "Render teardown clears activity records and marker ownership."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "one") "ok")))
    (let* ((record (gethash 0 pi-coding-agent--activity-table))
           (start (pi-coding-agent--activity-record-start-marker record)))
      (pi-coding-agent--clear-render-artifacts)
      (should-not (marker-position start))
      (should (= 0 (hash-table-count pi-coding-agent--activity-table))))))

(ert-deftest pi-coding-agent-test-tool-activity-navigation-unchanged ()
  "Grouped activity does not alter turn heading ordinals."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "one") "ok"))
     '(("bash" (:command "two") "ok")))
    (let ((headings (pi-coding-agent--collect-you-headings)))
      (should (= 2 (length headings)))
      (goto-char (car headings))
      (should (= 0 (pi-coding-agent--user-turn-index-at-point headings)))
      (goto-char (cadr headings))
      (should (= 1 (pi-coding-agent--user-turn-index-at-point headings))))))

(ert-deftest pi-coding-agent-test-tool-activity-cues-are-single-column ()
  "Disclosure and status cues occupy one terminal column."
  (dolist (cue '("▸" "▾" "◌" "›" "!" "·"))
    (should (= 1 (string-width cue)))))

(ert-deftest pi-coding-agent-test-tool-activity-buttons-have-static-faces ()
  "Grouped rows use static faces without button underlines."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "one") "ok")))
    (pi-coding-agent-tool-activity-test--goto-group)
    (let ((group-button (button-at (point))))
      (should group-button)
      (should-not (button-get group-button 'face))
      (should (eq 'pi-coding-agent-activity-label
                  (get-text-property (point) 'font-lock-face))))
    (pi-coding-agent-toggle-tool-section)
    (pi-coding-agent-tool-activity-test--goto "    bash")
    (let ((tool-button (button-at (point))))
      (should tool-button)
      (should-not (button-get tool-button 'face))
      (should (eq 'pi-coding-agent-activity-tool-name
                  (get-text-property (+ (point) 4) 'font-lock-face))))))

(ert-deftest pi-coding-agent-test-activity-icon-set-has-safe-width ()
  "Every configured emoji is width two and needs no variation selector."
  (let ((forbidden '("🛠" "⚠" "⚙")))
    (dolist (entry pi-coding-agent--activity-icon-set)
      (let ((marker (cdr entry)))
        (should (= 2 (string-width marker)))
        (should-not (string-match-p (string #xFE0F) marker))
        (should-not (member marker forbidden))))))

(ert-deftest pi-coding-agent-test-activity-type-marker-styles ()
  "Activity type markers depend only on the explicit style."
  (dolist (case '((emoji "🔧" "💭")
                  (text "Tools" "Think")
                  (none nil nil)))
    (let ((pi-coding-agent-activity-icon-style (car case)))
      (cl-letf (((symbol-function 'find-font)
                 (lambda (&rest _) (ert-fail "font probing is forbidden")))
                ((symbol-function 'display-graphic-p)
                 (lambda (&rest _) (ert-fail "frame probing is forbidden"))))
        (should (equal (nth 1 case)
                       (pi-coding-agent--activity-type-marker 'tools)))
        (should (equal (nth 2 case)
                       (pi-coding-agent--activity-type-marker 'thinking)))))))

(ert-deftest pi-coding-agent-test-activity-icon-style-row-shapes ()
  "Completed and live group rows use the three exact marker shapes."
  (let ((files (make-hash-table :test 'equal)))
    (dotimes (index 6)
      (puthash (format "file-%d" index) t files))
    (let ((record
           (pi-coding-agent--make-activity-record
            :turn-id 0 :calls 6 :running nil :failed 0
            :file-counts files :added 0 :removed 0
            :test-passed 0 :test-failed 0)))
      (dolist (case '((emoji "▸ 🔧 · edited 6" "◌ 🔧 · working")
                      (text "▸ Tools · edited 6" "◌ Tools · working")
                      (none "▸ edited 6" "◌ working")))
        (let ((pi-coding-agent-activity-icon-style (car case)))
          (should (string-prefix-p
                   (nth 1 case)
                   (substring-no-properties
                    (pi-coding-agent--activity-group-text record))))
          (setf (pi-coding-agent--activity-record-live record) t)
          (should (equal
                   (nth 2 case)
                   (substring-no-properties
                    (pi-coding-agent--activity-group-text record))))
          (setf (pi-coding-agent--activity-record-live record) nil))))))

(ert-deftest pi-coding-agent-test-activity-icon-style-alignment-and-budget ()
  "All styles share a total row budget and a four-column tool indent."
  (let ((files (make-hash-table :test 'equal)))
    (puthash "a.el" t files)
    (let ((record
           (pi-coding-agent--make-activity-record
            :turn-id 0 :calls 1 :running nil :failed 0
            :file-counts files :added 0 :removed 0
            :test-passed 0 :test-failed 0)))
      (dolist (style '(emoji text none))
        (let ((pi-coding-agent-activity-icon-style style))
          (cl-letf (((symbol-function 'pi-coding-agent--activity-line-budget)
                     (lambda () 16)))
            (let ((row (pi-coding-agent--activity-group-text record)))
              (should (<= (string-width row) 16))
              (should (string-match-p "edited 1" row))))))
      (let* ((pi-coding-agent-activity-icon-style 'emoji)
             (row (pi-coding-agent--activity-group-text record)))
        (should (= 4 (string-width
                      (substring row 0 (string-match " · " row))))))
      (dolist (style '(emoji text none))
        (let* ((pi-coding-agent-activity-icon-style style)
               (line
                (pi-coding-agent--activity-tool-line
                 '(:status running :tool-name "bash" :target "test"))))
          (should (equal "bash" (substring-no-properties line 4 8)))
          (should (= 4 (string-width (substring line 0 4))))
          (should (string-prefix-p "  ◌ "
                                   (substring-no-properties line))))))))

(ert-deftest pi-coding-agent-test-activity-icon-style-is-projection-only ()
  "Changing style leaves existing text alone until an explicit rebuild."
  (pi-coding-agent-tool-activity-test--with-chat
    (let ((pi-coding-agent-activity-icon-style 'text))
      (pi-coding-agent-tool-activity-test--render-history
       '(("bash" (:command "one") "ok")))
      (let ((before (buffer-string)))
        (setq pi-coding-agent-activity-icon-style 'emoji)
        (should (equal before (buffer-string)))
        (pi-coding-agent--display-session-history
         pi-coding-agent--canonical-messages (current-buffer))
        (should (string-match-p
                 "🔧" (pi-coding-agent-tool-activity-test--visible)))))))

(ert-deftest pi-coding-agent-test-activity-type-and-status-stay-separate ()
  "Failure changes cue and text faces, not the type marker."
  (let ((pi-coding-agent-activity-icon-style 'emoji)
        (files (make-hash-table :test 'equal)))
    (let* ((success
            (pi-coding-agent--make-activity-record
             :turn-id 0 :calls 1 :running nil :failed 0
             :file-counts files :added 0 :removed 0
             :test-passed 0 :test-failed 0))
           (failure
            (pi-coding-agent--make-activity-record
             :turn-id 1 :calls 1 :running nil :failed 1
             :file-counts files :added 0 :removed 0
             :test-passed 0 :test-failed 0))
           (success-row (pi-coding-agent--activity-group-text success))
           (failure-row (pi-coding-agent--activity-group-text failure))
           (tool-row
            (pi-coding-agent--activity-tool-line
             '(:status failed :tool-name "bash" :summary "boom"))))
      (should (string-match-p "🔧" success-row))
      (should (string-match-p "🔧" failure-row))
      (should (string-match-p "1 call failed" failure-row))
      (should (eq 'pi-coding-agent-activity-label
                  (get-text-property 0 'font-lock-face success-row)))
      (should (eq 'pi-coding-agent-activity-failed
                  (get-text-property 0 'font-lock-face failure-row)))
      (should (eq 'pi-coding-agent-activity-failed
                  (get-text-property 2 'font-lock-face tool-row))))))

(defun pi-coding-agent-tool-activity-test--thought-snapshot ()
  "Return the current turn's ordered lightweight thought previews."
  (mapcar (lambda (thought) (plist-get thought :preview))
          (pi-coding-agent--reading-turn-thoughts (pi-coding-agent--reading-current))))

(defun pi-coding-agent-tool-activity-test--segment-snapshot ()
  "Return canonical identity and counts for the current turn and segments."
  (let ((turn (pi-coding-agent--activity-record-for-turn
               pi-coding-agent--current-activity-turn)))
    (list (pi-coding-agent--activity-aggregates turn)
          (mapcar
           (lambda (segment)
             (list (pi-coding-agent--activity-record-turn-id segment)
                   (pi-coding-agent--activity-record-segment-ordinal segment)
                   (mapcar (lambda (tool) (plist-get tool :tool-call-id))
                           (pi-coding-agent--activity-record-tools segment))
                   (pi-coding-agent--activity-aggregates segment)))
           (pi-coding-agent--activity-record-segments turn)))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-shared-live-history ()
  "Canonical segment ownership survives delayed starts and late duplicates."
  (let ((fixture (pi-coding-agent-progress-fixture))
        (pi-coding-agent-thinking-activity-display 'grouped)
        (pi-coding-agent-thinking-display 'hidden)
        live-snapshot live-text live-thoughts)
    (pi-coding-agent-tool-activity-test--with-chat
      (setq-local pi-coding-agent--output-progress-display 'segmented)
      (dolist (event (plist-get fixture :events))
        (pi-coding-agent--handle-display-event event))
      (setq live-snapshot (pi-coding-agent-tool-activity-test--segment-snapshot)
            live-text (pi-coding-agent-tool-activity-test--visible)
            live-thoughts (pi-coding-agent-tool-activity-test--thought-snapshot))
      (let* ((turn (pi-coding-agent--activity-record-for-turn 0))
             (segments (pi-coding-agent--activity-record-segments turn)))
        (should (= 1 (hash-table-count pi-coding-agent--activity-table)))
        (should (= 2 (length segments)))
        (should (eq (car segments) (pi-coding-agent--activity-record-for-tool "segment-1")))
        (should (eq (car segments) (pi-coding-agent--activity-record-for-tool "segment-2")))
        (should (eq (cadr segments) (pi-coding-agent--activity-record-for-tool "segment-3")))
        (should (eq (pi-coding-agent--activity-tool turn "segment-1")
                    (pi-coding-agent--activity-tool (car segments) "segment-1")))
        (should (= 1 (plist-get (car live-snapshot) :files)))
        (should (= 3 (plist-get (car live-snapshot) :started)))
        (should (= 3 (plist-get (car live-snapshot) :completed)))
        (should (= 1 (plist-get (car live-snapshot) :mcp)))
        (dolist (segment segments)
          (should (= 1 (plist-get (pi-coding-agent--activity-aggregates segment) :files))))
        (should-not (string-match-p "working\\|PRIVATE PAYLOAD" live-text))
        (should (= 2 (pi-coding-agent-tool-activity-test--count "Tools")))
        (should (string-match-p "Body A[[:ascii:][:nonascii:]]*Tools[[:ascii:][:nonascii:]]*Body B[[:ascii:][:nonascii:]]*Tools[[:ascii:][:nonascii:]]*Body C" live-text))))
    (pi-coding-agent-tool-activity-test--with-chat
      (setq-local pi-coding-agent--output-progress-display 'segmented)
      (let ((publications 0))
        (cl-letf (((symbol-function 'pi-coding-agent--tool-detail-text)
                   (lambda (&rest _) (ert-fail "History materialized payload")))
                  ((symbol-function 'pi-coding-agent--update-progress)
                   (lambda (aggregate &rest _)
                     (when aggregate (cl-incf publications)))))
          (pi-coding-agent--display-session-history
           (plist-get fixture :messages) (current-buffer)))
        (should (= 1 publications))
        (should-not pi-coding-agent--progress-origin))
      (should (equal live-snapshot (pi-coding-agent-tool-activity-test--segment-snapshot)))
      (should (equal live-thoughts (pi-coding-agent-tool-activity-test--thought-snapshot)))
      (should (= 3 (length live-thoughts)))
      (should (= 2 (pi-coding-agent-tool-activity-test--count "Tools")))
      (should-not (string-match-p "PRIVATE PAYLOAD" (pi-coding-agent-tool-activity-test--visible)))
      (let ((positions (mapcar (lambda (text)
                                (pi-coding-agent-tool-activity-test--goto text) (point))
                              '("Body A" "Body B" "Body C"))))
        (should (apply #'< positions))))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-legacy-rebuild-roundtrip ()
  "Mode rebuild changes projection only and preserves canonical message data."
  (pi-coding-agent-tool-activity-test--with-chat
    (let* ((messages (plist-get (pi-coding-agent-progress-fixture) :messages))
           (original (copy-tree messages t)))
      (setq-local pi-coding-agent--output-progress-display 'legacy)
      (pi-coding-agent--display-session-history messages (current-buffer))
      (should (= 1 (pi-coding-agent-tool-activity-test--count "Tools")))
      (setq-local pi-coding-agent--output-progress-display 'segmented)
      (pi-coding-agent--rerender-canonical-history)
      (should (= 2 (pi-coding-agent-tool-activity-test--count "Tools")))
      (should (eq messages pi-coding-agent--canonical-messages))
      (should (equal original messages))
      (setq-local pi-coding-agent--output-progress-display 'legacy)
      (pi-coding-agent--rerender-canonical-history)
      (should (= 1 (pi-coding-agent-tool-activity-test--count "Tools")))
      (should (eq messages pi-coding-agent--canonical-messages))
      (should (equal original messages))
      (should (= 1 (hash-table-count pi-coding-agent--activity-table)))
      (should-not (pi-coding-agent--activity-record-segments
                   (pi-coding-agent--activity-record-for-turn 0))))))

(ert-deftest pi-coding-agent-test-tool-activity-steering-progress-new-turn ()
  "Canonical steering resets progress; a local echo preserves request timing."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (let ((clock 100.0))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock)))
        (pi-coding-agent--progress-start)
        (pi-coding-agent--display-user-message "first request")
        (pi-coding-agent--set-activity-phase "running")
        (pi-coding-agent--activity-finish-tool "failed" "bash" nil [] nil t t)
        (should (= 1 (plist-get pi-coding-agent--progress-aggregate :failed)))
        (setq clock 105.0)
        (pi-coding-agent--handle-display-event
         '(:type "message_start" :message (:role "user" :content [(:type "text" :text "steering request")])))
        (pi-coding-agent--handle-display-event
         '(:type "message_update" :assistantMessageEvent (:type "text_delta" :delta "text only")))
        (should (= 105.0 pi-coding-agent--progress-origin))
        (should-not pi-coding-agent--progress-aggregate)
        (should-not (string-match-p "failed\\|calls\\|!1" (pi-coding-agent--progress-string)))
        ;; A submitted local message has already started its clock and turn.
        (setq clock 110.0)
        (pi-coding-agent--progress-start)
        (setq pi-coding-agent--local-user-message "local request")
        (pi-coding-agent--display-user-message "local request")
        (setq clock 115.0)
        (pi-coding-agent--handle-display-event
         '(:type "message_start" :message (:role "user" :content [(:type "text" :text "local request")])))
        (should (= 110.0 pi-coding-agent--progress-origin))))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-pending-and-semantic-refresh ()
  "Pending calls are not started; duplicate and payload events do not rewrite."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (pi-coding-agent--display-user-message "pending")
    (pi-coding-agent--activity-start-tool "p" "bash" '(:command "one; two") t t)
    (let* ((record (pi-coding-agent--activity-record-for-tool "p"))
           (aggregate (pi-coding-agent--activity-aggregates record)))
      (should (= 1 (plist-get aggregate :calls)))
      (should (= 0 (plist-get aggregate :started)))
      (should-not (plist-get aggregate :running))
      (should (= 1 (plist-get aggregate :bash)))
      (pi-coding-agent--activity-start-tool "p" "bash" '(:command "one; two") t)
      (should (= 1 (plist-get (pi-coding-agent--activity-aggregates record) :started)))
      (let ((writes 0))
        (cl-letf (((symbol-function 'pi-coding-agent--activity-render-record)
                   (lambda (&rest _) (cl-incf writes)))
                  ((symbol-function 'pi-coding-agent--refresh-header)
                   (lambda (&rest _) (ert-fail "Progress requested stats RPC"))))
          (dotimes (_ 20)
            (pi-coding-agent--activity-start-tool "p" "bash" '(:command "one; two") t)
            (pi-coding-agent--handle-display-event
             '(:type "tool_execution_update" :toolCallId "p" :partialResult (:text "more"))))
          (should (= 0 writes))))
      (pi-coding-agent--activity-finish-tool "p" "bash" '(:command "one; two") [] nil t t)
      (pi-coding-agent--activity-start-tool "retry" "bash" '(:command "one; two") t)
      (pi-coding-agent--activity-finish-tool "retry" "bash" nil [] nil nil t)
      (let ((turn (pi-coding-agent--activity-record-for-turn 0)))
        (should (= 1 (plist-get (pi-coding-agent--activity-aggregates turn) :failed)))
        (should (= 2 (plist-get (pi-coding-agent--activity-aggregates turn) :completed)))))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-classification-explicit-only ()
  "Only direct registered MCP metadata classifies an MCP call."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (pi-coding-agent--display-user-message "metadata")
    (cl-loop for metadata in '(nil (:isMcp :false) (:toolType "mcp")
                              (:tool-type mcp) (:isMcp t)
                              (:registration (:toolType "mcp"))
                              (:tool (:tool-type mcp))
                              (:metadata (:nested (:toolType "mcp"))))
             for id from 0 do
             (pi-coding-agent--activity-start-tool (number-to-string id) "mcp__looks_like_mcp"
                                                  nil t nil metadata))
    (let ((aggregate (pi-coding-agent--activity-aggregates
                      (pi-coding-agent--activity-record-for-turn 0))))
      (should (= 5 (plist-get aggregate :mcp)))
      (should (= 3 (plist-get aggregate :other))))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-group-interaction-local ()
  "Segment metadata scopes TAB, RET and click without altering turn identity."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (dolist (event (plist-get (pi-coding-agent-progress-fixture) :events))
      (pi-coding-agent--handle-display-event event))
    (let* ((first (pi-coding-agent--activity-record-for-tool "segment-1"))
           (second (pi-coding-agent--activity-record-for-tool "segment-3")))
      (goto-char (pi-coding-agent--activity-record-start-marker first))
      (should (= 0 (get-text-property (point) 'pi-coding-agent-activity-turn)))
      (should (= 1 (get-text-property (point) 'pi-coding-agent-activity-segment)))
      (should (eq (key-binding (kbd "TAB")) #'pi-coding-agent-toggle-tool-section))
      (should (eq (key-binding (kbd "<tab>")) #'pi-coding-agent-toggle-tool-section))
      ;; Resolve the real button keymap, then dispatch in this temp buffer.
      ;; Keyboard macros themselves operate the batch selected window instead.
      (call-interactively (key-binding (kbd "TAB")))
      (should (pi-coding-agent--activity-record-expanded first))
      (should-not (pi-coding-agent--activity-record-expanded second))
      (goto-char (pi-coding-agent--activity-record-start-marker second))
      (pi-coding-agent--activity-toggle-group-button (button-at (point)))
      (should (pi-coding-agent--activity-record-expanded second))
      (pi-coding-agent--activity-finish-tool "segment-1" "edit"
                                          '(:path "/tmp/pi-tool-activity/shared.el")
                                          [] '(:diff "+ 1 changed") nil t)
      (should (pi-coding-agent--activity-record-expanded first))
      (should (pi-coding-agent--activity-record-expanded second)))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-whitespace-agent-boundary ()
  "Thinking, whitespace and agent boundaries do not manufacture segments."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (pi-coding-agent--display-user-message "boundaries")
    (pi-coding-agent--activity-start-tool "one" "bash" nil t t)
    (pi-coding-agent--activity-text-boundary " \n\t" t)
    (pi-coding-agent--activity-finalize-live-turn)
    (pi-coding-agent--activity-start-tool "two" "bash" nil t t)
    (let ((turn (pi-coding-agent--activity-record-for-turn 0)))
      (should (= 1 (length (pi-coding-agent--activity-record-segments turn))))
      (should (eq (pi-coding-agent--activity-record-for-tool "one")
                  (pi-coding-agent--activity-record-for-tool "two"))))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-read-edit-dedup ()
  "Successful path sets are independent and operations accumulate per call."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (pi-coding-agent--display-user-message "paths")
    (cl-loop for id in '("read-one" "read-two" "read-failed") do
             (pi-coding-agent--activity-finish-tool
              id "read" '(:path "/tmp/pi-tool-activity/shared.el") [] nil
              (equal id "read-failed") t))
    (cl-loop for id in '("edit-one" "edit-two") do
             (pi-coding-agent--activity-finish-tool
              id "edit" '(:path "/tmp/pi-tool-activity/shared.el") []
              '(:diff "+ 1 new\n- 1 old") nil t))
    (pi-coding-agent--activity-finish-tool
     "unknown-path" "read" nil [] nil nil t)
    (let ((aggregate (pi-coding-agent--activity-aggregates
                      (pi-coding-agent--activity-record-for-turn 0))))
      (should (= 1 (plist-get aggregate :read)))
      (should (= 1 (plist-get aggregate :files)))
      (should (= 1 (plist-get aggregate :failed)))
      (should (= 2 (plist-get aggregate :added)))
      (should (= 2 (plist-get aggregate :removed))))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-clear-releases-segments ()
  "Cleanup releases segment markers and the existing session indexes."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (pi-coding-agent--display-user-message "cleanup")
    (pi-coding-agent--activity-start-tool "one" "bash" nil t)
    (let ((marker (pi-coding-agent--activity-record-start-marker
                   (pi-coding-agent--activity-record-for-tool "one"))))
      (pi-coding-agent--activity-clear-state)
      (should-not (marker-position marker))
      (should (= 0 (hash-table-count pi-coding-agent--activity-table)))
      (should (= 0 (hash-table-count pi-coding-agent--tool-activity-index))))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-tiny-failure-budget ()
  "Tiny segmented rows preserve failure and complete help text."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (pi-coding-agent--display-user-message "narrow")
    (pi-coding-agent--activity-finish-tool "bad" "bash" nil [] nil t t)
    (let ((record (pi-coding-agent--activity-record-for-tool "bad")))
      (dolist (width '(1 5 12 72))
        (cl-letf (((symbol-function 'pi-coding-agent--activity-line-budget) (lambda () width)))
          (let ((text (pi-coding-agent--activity-group-text record)))
            (should (<= (string-width text) width))
            (should (string-match-p "failed\\|!" text))
            (should (string-match-p "1 call failed" (get-text-property 0 'help-echo text)))))))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-summary-keeps-per-tool-ui ()
  "Explicit summary display remains per-tool while fixed counts are recorded."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented
                pi-coding-agent--tool-activity-display 'summary)
    (pi-coding-agent--display-user-message "summary")
    (pi-coding-agent-tool-activity-test--render-tool "summary" "bash" '(:command "echo ok") "ok")
    (should-not (text-property-any (point-min) (point-max) 'pi-coding-agent-activity-group t))
    (should (string-match-p "\\$ echo ok" (pi-coding-agent-tool-activity-test--visible)))
    (let ((turn (pi-coding-agent--activity-record-for-turn 0)))
      (should-not (pi-coding-agent--activity-record-segments turn))
      (should (= 1 (plist-get (pi-coding-agent--activity-aggregates turn) :completed))))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-summary-interrupt ()
  "Summary interruption settles running calls without inserting groups."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented
                pi-coding-agent--tool-activity-display 'summary)
    (pi-coding-agent--display-user-message "exit")
    (pi-coding-agent--activity-start-tool "pending" "bash" nil t t)
    (pi-coding-agent--activity-start-tool "running" "bash" nil t)
    (let ((turn (pi-coding-agent--activity-record-for-turn 0)))
      (pi-coding-agent--activity-interrupt-live-turn)
      (should-not (pi-coding-agent--activity-record-live turn))
      (should (eq 'pending (plist-get (pi-coding-agent--activity-tool turn "pending") :status)))
      (should (eq 'failed (plist-get (pi-coding-agent--activity-tool turn "running") :status)))
      (should (= 1 (plist-get (pi-coding-agent--activity-aggregates turn) :started)))
      (should (= 1 (plist-get (pi-coding-agent--activity-aggregates turn) :completed)))
      (should-not (plist-get (pi-coding-agent--activity-aggregates turn) :running))
      (should-not (text-property-any (point-min) (point-max) 'pi-coding-agent-activity-group t)))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-blocks-thinking-stub ()
  "Blocks Thinking uses a stable record entry, preserving legacy fallback."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (let ((pi-coding-agent-activity-icon-style 'text))
      (should (equal (substring-no-properties (pi-coding-agent--thinking-live-stub))
                     "▸ Think · 思考记录")))
    (let ((pi-coding-agent-activity-icon-style 'none))
      (should (equal (substring-no-properties (pi-coding-agent--thinking-live-stub))
                     "▸ 思考记录")))
    (setq-local pi-coding-agent--output-progress-display 'legacy)
    (should (string-match-p "working" (pi-coding-agent--thinking-live-stub)))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-late-old-text-index ()
  "Late deltas in an earlier canonical text block cannot split later tools."
  (let* ((blocks [(:type "text" :text "A later")
                  (:type "toolCall" :id "a" :name "bash" :arguments (:command "a"))
                  (:type "toolCall" :id "b" :name "bash" :arguments (:command "b"))])
         (message (list :role "assistant" :content blocks)) live)
    (pi-coding-agent-tool-activity-test--with-chat
      (setq-local pi-coding-agent--output-progress-display 'segmented)
      (pi-coding-agent--display-user-message "old text")
      (pi-coding-agent--handle-display-event '(:type "agent_start"))
      (pi-coding-agent--handle-display-event
       '(:type "message_start" :message (:role "assistant" :content [])))
      (dolist (entry '(("text_delta" 0 "A") ("toolcall_start" 1)
                       ("text_delta" 0 " later") ("toolcall_start" 2)))
        (pi-coding-agent--handle-display-event
         (list :type "message_update" :message message
               :assistantMessageEvent
               (list :type (car entry) :contentIndex (cadr entry) :delta (nth 2 entry)))))
      (setq live (pi-coding-agent-tool-activity-test--segment-snapshot))
      (should (= 1 (length (cadr live))))
      (should (= 0 (plist-get (car live) :started)))
      (should (eq (pi-coding-agent--activity-record-for-tool "a")
                  (pi-coding-agent--activity-record-for-tool "b"))))
    (pi-coding-agent-tool-activity-test--with-chat
      (setq-local pi-coding-agent--output-progress-display 'segmented)
      (pi-coding-agent--display-session-history
       (vector '(:role "user" :content "old text") message) (current-buffer))
      (should (equal live (pi-coding-agent-tool-activity-test--segment-snapshot))))))

(ert-deftest pi-coding-agent-test-tool-activity-segmented-newest-text-first-nonblank ()
  "Whitespace becoming nonblank closes once; raw fallback still closes tools."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (pi-coding-agent--display-user-message "whitespace")
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--activity-reconcile-canonical
     '(:role "assistant" :content [(:type "toolCall" :id "one" :name "bash")]) 0)
    (dolist (text '(" " " explanation" " explanation more"))
      (pi-coding-agent--handle-display-event
       (list :type "message_update"
             :message (list :role "assistant" :content
                            (vector '(:type "toolCall" :id "one" :name "bash")
                                    (list :type "text" :text text)))
             :assistantMessageEvent (list :type "text_delta" :contentIndex 1 :delta text))))
    (pi-coding-agent--activity-start-tool "two" "bash" nil t t)
    (should-not (eq (pi-coding-agent--activity-record-for-tool "one")
                    (pi-coding-agent--activity-record-for-tool "two")))
    ;; The direct renderer has no canonical message/index evidence.
    (pi-coding-agent--display-message-delta "raw boundary")
    (pi-coding-agent--activity-start-tool "three" "bash" nil t t)
    (should-not (eq (pi-coding-agent--activity-record-for-tool "two")
                    (pi-coding-agent--activity-record-for-tool "three")))))

(ert-deftest pi-coding-agent-test-tool-activity-display-handler-process-ownership ()
  "Stale and transition processes cannot mutate the current chat projection."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (let ((old (make-pipe-process :name "pi-progress-stale" :noquery t))
          (current (make-pipe-process :name "pi-progress-current" :noquery t)))
      (unwind-protect
          (progn
            (setq-local pi-coding-agent--process current)
            (pi-coding-agent--display-user-message "ownership")
            (dolist (process (list old current))
              (process-put process 'pi-coding-agent-chat-buffer (current-buffer))
              (pi-coding-agent--register-display-handler process))
            (let ((before (buffer-string))
                  (state (copy-tree pi-coding-agent--state))
                  (cache (copy-tree pi-coding-agent--progress-cache)))
              (dolist (transition '(nil t))
                (setq-local pi-coding-agent--session-transition-active transition
                            pi-coding-agent--session-transition-process (and transition old))
                (pi-coding-agent--handle-event
                 old '(:type "tool_execution_start" :toolCallId "stale" :toolName "bash")))
              (should (equal before (buffer-string)))
              (should (equal state pi-coding-agent--state))
              (should (equal cache pi-coding-agent--progress-cache))
              (should-not (pi-coding-agent--activity-record-for-tool "stale")))
            (pi-coding-agent--handle-event
             current '(:type "tool_execution_start" :toolCallId "current" :toolName "bash"))
            (should (= 1 (plist-get (pi-coding-agent--activity-aggregates
                                    (pi-coding-agent--activity-record-for-turn 0)) :started)))
            (should (equal "running" pi-coding-agent--activity-phase)))
        (setq-local pi-coding-agent--process nil
                    pi-coding-agent--session-transition-process nil)
        (delete-process old)
        (delete-process current)))))

(ert-deftest pi-coding-agent-test-tool-activity-duplicate-events-preserve-reply-phase ()
  "Duplicate completed starts and ends preserve reply phase and cached state."
  (dolist (mode '(legacy segmented))
    (pi-coding-agent-tool-activity-test--with-chat
      (setq-local pi-coding-agent--output-progress-display mode)
      (pi-coding-agent--display-user-message "duplicates")
      (pi-coding-agent-tool-activity-test--render-tool "done" "bash" '(:command "true") "ok")
      (pi-coding-agent--set-activity-phase "replying")
      (let ((cache (copy-tree pi-coding-agent--progress-cache))
            (state (copy-tree pi-coding-agent--state)))
        (pi-coding-agent--handle-display-event
         '(:type "tool_execution_end" :toolCallId "done" :toolName "bash"
           :result (:content [(:type "text" :text "ok")])))
        (pi-coding-agent--handle-display-event
         '(:type "tool_execution_start" :toolCallId "done" :toolName "bash" :args (:command "true")))
        (should (equal "replying" pi-coding-agent--activity-phase))
        (should (equal cache pi-coding-agent--progress-cache))
        (should (equal state pi-coding-agent--state))))))

(ert-deftest pi-coding-agent-test-tool-activity-old-turn-result-preserves-current-phase ()
  "Late old-turn results update their segment but never new-turn phase/cache."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (pi-coding-agent--display-user-message "old")
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "old" :toolName "bash"))
    (let ((owner (pi-coding-agent--activity-record-for-tool "old")))
      (pi-coding-agent--display-user-message "new")
      (pi-coding-agent-tool-activity-test--render-tool "new" "bash" nil "new")
      (pi-coding-agent--set-activity-phase "replying")
      (let ((cache (copy-tree pi-coding-agent--progress-cache))
            (state (copy-tree pi-coding-agent--state)))
        (pi-coding-agent--handle-display-event
         '(:type "tool_execution_end" :toolCallId "old" :toolName "bash"
           :result (:content [(:type "text" :text "late")]) :isError t))
        (pi-coding-agent--handle-display-event
         '(:type "tool_execution_start" :toolCallId "old" :toolName "bash"))
        (should (equal "replying" pi-coding-agent--activity-phase))
        (should (equal cache pi-coding-agent--progress-cache))
        (should (equal state pi-coding-agent--state))
        (should (eq owner (pi-coding-agent--activity-record-for-tool "old")))
        (should (= 1 (plist-get (pi-coding-agent--activity-aggregates owner) :failed)))))))

(ert-deftest pi-coding-agent-test-tool-activity-genuine-end-keeps-parallel-running ()
  "Genuine execution end retains legacy phase and parallel progress override."
  (pi-coding-agent-tool-activity-test--with-chat
    (setq-local pi-coding-agent--output-progress-display 'segmented)
    (pi-coding-agent--progress-start)
    (pi-coding-agent--display-user-message "parallel")
    (dolist (id '("one" "two"))
      (pi-coding-agent--handle-display-event
       (list :type "tool_execution_start" :toolCallId id :toolName "bash")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_end" :toolCallId "one" :toolName "bash" :result (:content [])))
    (should (equal "thinking" pi-coding-agent--activity-phase))
    (should (equal "running" (pi-coding-agent--progress-phase)))
    (should (= 1 (length (plist-get pi-coding-agent--progress-aggregate :running))))))

(provide 'pi-coding-agent-tool-activity-test)
;;; pi-coding-agent-tool-activity-test.el ends here
