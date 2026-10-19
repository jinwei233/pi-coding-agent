;;; pi-coding-agent-tool-activity-test.el --- Grouped tool activity -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract tests for one lightweight tool activity group per user turn.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pi-coding-agent)

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
      (should (string-match-p "1 file" visible))
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
      (should (string-match-p "1 file" settled))
      (should (string-match-p "2 calls" settled)))))

(ert-deftest pi-coding-agent-test-tool-activity-failure-has-priority ()
  "Failures remain visible in the folded group."
  (pi-coding-agent-tool-activity-test--with-chat
    (pi-coding-agent-tool-activity-test--render-history
     '(("bash" (:command "false") "boom\nprivate tail" nil t)
       ("read" (:path "/tmp/pi-tool-activity/a.el") "ok")))
    (let ((visible (pi-coding-agent-tool-activity-test--visible)))
      (should (string-match-p "1 failed" visible))
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
    (let (detail-buffer)
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
      (should (string-match-p "1 failed" visible)))))

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
      (dolist (case '((emoji "▸ 🔧 · 6 files" "◌ 🔧 · working")
                      (text "▸ Tools · 6 files" "◌ Tools · working")
                      (none "▸ 6 files" "◌ working")))
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
              (should (string-match-p "1 file" row))))))
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
      (should (string-match-p "1 failed" failure-row))
      (should (eq 'pi-coding-agent-activity-label
                  (get-text-property 0 'font-lock-face success-row)))
      (should (eq 'pi-coding-agent-activity-failed
                  (get-text-property 0 'font-lock-face failure-row)))
      (should (eq 'pi-coding-agent-activity-failed
                  (get-text-property 2 'font-lock-face tool-row))))))

(provide 'pi-coding-agent-tool-activity-test)
;;; pi-coding-agent-tool-activity-test.el ends here
