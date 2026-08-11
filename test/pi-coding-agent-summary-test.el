;;; pi-coding-agent-summary-test.el --- Tool summary tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pi-coding-agent)

(defun pi-coding-agent-summary-test--messages
    (name args content &optional details error)
  "Return canonical messages for NAME, ARGS, CONTENT, DETAILS and ERROR."
  (vector
   (list :role "assistant"
         :content
         (vector (list :type "toolCall" :id "call-1"
                       :name name :arguments args)))
   (list :role "toolResult" :toolCallId "call-1" :toolName name
         :content (vector (list :type "text" :text content))
         :details details :isError (and error t))))

(defmacro pi-coding-agent-summary-test--with-history (messages &rest body)
  "Render MESSAGES in a temporary chat buffer, then evaluate BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (pi-coding-agent-chat-mode)
     (pi-coding-agent--set-chat-session-identity "/tmp/pi-summary/")
     (pi-coding-agent--display-session-history ,messages (current-buffer))
     ,@body))

(ert-deftest pi-coding-agent-test-tool-summary-builtins-hide-full-payloads ()
  "Successful built-ins show metadata without their complete payload."
  (dolist (case
           `(("edit" (:path "src/a.el") "done"
              (:diff "+ 10 alpha\n- 11 beta\nSECRET_EDIT") "SECRET_EDIT"
              "\\+1 -1")
             ("write" (:path "src/a.el" :content "line1\nSECRET_WRITE")
              "wrote" nil "SECRET_WRITE" "2 lines")
             ("read" (:path "src/a.el" :offset 20) "line20\nSECRET_READ"
              nil "SECRET_READ" "2 lines")
             ("bash" (:command "run-tests")
              ,(concat (mapconcat (lambda (n) (format "old-%d" n))
                                  (number-sequence 1 20) "\n")
                       "\nTAIL_BASH")
              nil "\nold-1\n" "hidden")))
    (pcase-let ((`(,name ,args ,content ,details ,hidden ,summary) case))
      (pi-coding-agent-summary-test--with-history
          (pi-coding-agent-summary-test--messages
           name args content details)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p summary text))
          (should-not (string-match-p hidden text))
          (should (string-match-p "TAB" text)))))))

(ert-deftest pi-coding-agent-test-tool-detail-pair-prefers-canonical-and-falls-back-live ()
  "Pair lookup uses canonical data first and live data for a missing id."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let* ((canonical
            (pi-coding-agent-summary-test--messages
             "read" '(:path "canonical") "canonical body"))
           (live-call '(:type "toolCall" :id "live" :name "write"
                        :arguments (:path "live" :content "live body")))
           (live-result '(:role "toolResult" :toolCallId "live"
                          :toolName "write" :content [])))
      (pi-coding-agent--set-canonical-messages canonical)
      (puthash "live" (list :tool-call live-call :tool-result live-result)
               pi-coding-agent--transient-tool-pairs)
      (should (equal "canonical body"
                     (pi-coding-agent--tool-detail-text
                      (pi-coding-agent--resolve-tool-detail-pair "call-1"))))
      (should (equal "live body"
                     (pi-coding-agent--tool-detail-text
                      (pi-coding-agent--resolve-tool-detail-pair "live")))))))

(ert-deftest pi-coding-agent-test-tool-end-indexes-live-pair-before-agent-end ()
  "A completed live tool can resolve details before canonical refresh."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "live-1"
       :toolName "write"
       :args (:path "/tmp/live" :content "LIVE_CONTENT")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_end" :toolCallId "live-1" :toolName "write"
       :result (:content [(:type "text" :text "wrote")]) :isError nil))
    (should (equal
             "LIVE_CONTENT"
             (pi-coding-agent--tool-detail-text
              (pi-coding-agent--resolve-tool-detail-pair "live-1"))))
    (pi-coding-agent--handle-display-event
     (list :type "agent_end"
           :messages
           [(:role "assistant"
             :content [(:type "toolCall" :id "live-1" :name "write"
                        :arguments
                        (:path "/tmp/live" :content "LIVE_CONTENT"))])
            (:role "toolResult" :toolCallId "live-1" :toolName "write"
             :content [(:type "text" :text "wrote")]
             :isError :json-false)]))
    (should (= 0 (hash-table-count pi-coding-agent--transient-tool-pairs)))))

(ert-deftest pi-coding-agent-test-agent-end-keeps-unpromoted-live-pair ()
  "A stale canonical vector must not discard an unpromoted live pair."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start" :toolCallId "live-1"
       :toolName "read" :args (:path "/tmp/live")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_end" :toolCallId "live-1" :toolName "read"
       :result (:content [(:type "text" :text "LIVE_CONTENT")])
       :isError nil))
    (pi-coding-agent--handle-display-event
     '(:type "agent_end" :messages []))
    (should (= 1 (hash-table-count pi-coding-agent--transient-tool-pairs)))
    (should (equal
             "LIVE_CONTENT"
             (pi-coding-agent--tool-detail-text
              (pi-coding-agent--resolve-tool-detail-pair "live-1"))))))

(ert-deftest pi-coding-agent-test-summary-artifacts-do-not-hold-payload ()
  "Collapsed summaries and cold metadata never retain hidden payload strings."
  (let ((secret (make-string (* 128 1024) ?z)))
    (pi-coding-agent-summary-test--with-history
        (pi-coding-agent-summary-test--messages
         "write" (list :path "large.txt" :content secret) "wrote")
      (let ((button (next-button (point-min))))
        (should button)
        (should-not (button-get button 'pi-coding-agent-full-content))
        (should-not
         (string-search secret
                        (format "%S"
                                (text-properties-at
                                 (button-start button))))))
      (pi-coding-agent--cool-completed-tool-blocks
       (seq-filter
        (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
        (overlays-in (point-min) (point-max))))
      (goto-char (point-min))
      (search-forward "write large.txt")
      (let* ((cold (pi-coding-agent--cold-tool-block-at-point))
             (metadata (plist-get cold :metadata)))
        (should cold)
        (should-not (string-search secret (format "%S" metadata)))))))

(ert-deftest pi-coding-agent-test-tool-errors-use-full-short-and-tail-long ()
  "Short errors remain visible while long errors use a tail summary."
  (pi-coding-agent-summary-test--with-history
      (pi-coding-agent-summary-test--messages
       "bash" '(:command "false") "short failure" nil t)
    (should (string-match-p "short failure" (buffer-string))))
  (let ((long-error (concat "HIDDEN_ERROR\n" (make-string 10000 ?x)
                            "\nvisible error tail")))
    (pi-coding-agent-summary-test--with-history
        (pi-coding-agent-summary-test--messages
         "bash" '(:command "false") long-error nil t)
      (should-not (string-match-p "HIDDEN_ERROR" (buffer-string)))
      (should (string-match-p "visible error tail" (buffer-string))))))

(ert-deftest pi-coding-agent-test-generic-summary-is-fixed-and-conservative ()
  "Generic summaries show bounded scalar fields and hide large fields."
  (let ((payload (make-string 10000 ?p)))
    (pi-coding-agent-summary-test--with-history
        (pi-coding-agent-summary-test--messages
         "search" (list :query "summary query" :payload payload)
         "ok" (list :data payload))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "summary query" text))
        (should (string-match-p "payload" text))
        (should (< (buffer-size) 2000))
        (should-not (string-match-p payload text))))))

(ert-deftest pi-coding-agent-test-detail-buffer-reuses-and-isolates-sessions ()
  "Detail buffers reuse within one session and isolate equal ids across chats."
  (let ((chat-a (generate-new-buffer " *pi-summary-a*"))
        (chat-b (generate-new-buffer " *pi-summary-b*")))
    (unwind-protect
        (let (a1 a2 b1)
          (with-current-buffer chat-a
            (pi-coding-agent-chat-mode)
            (pi-coding-agent--set-chat-session-identity "/tmp/a/")
            (pi-coding-agent--set-canonical-messages
             (pi-coding-agent-summary-test--messages
              "read" '(:path "a") "A detail"))
            (setq a1 (pi-coding-agent--open-tool-detail-buffer "call-1")
                  a2 (pi-coding-agent--open-tool-detail-buffer "call-1"))
            (should (eq a1 a2))
            (should (= 1 (hash-table-count
                          pi-coding-agent--tool-detail-buffers))))
          (with-current-buffer chat-b
            (pi-coding-agent-chat-mode)
            (pi-coding-agent--set-chat-session-identity "/tmp/b/")
            (pi-coding-agent--set-canonical-messages
             (pi-coding-agent-summary-test--messages
              "read" '(:path "b") "B detail"))
            (setq b1 (pi-coding-agent--open-tool-detail-buffer "call-1"))
            (should-not (eq a1 b1)))
          (kill-buffer a1)
          (with-current-buffer chat-a
            (should (= 0 (hash-table-count
                          pi-coding-agent--tool-detail-buffers)))))
      (dolist (buffer (list chat-a chat-b))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest pi-coding-agent-test-detail-buffer-lifecycle-cleanup ()
  "q, abort, retry, and chat teardown release registered detail buffers."
  (dolist (cleanup '(q abort retry chat-kill))
    (let ((chat (generate-new-buffer " *pi-summary-lifecycle*"))
          detail)
      (unwind-protect
          (progn
            (with-current-buffer chat
              (pi-coding-agent-chat-mode)
              (pi-coding-agent--set-canonical-messages
               (pi-coding-agent-summary-test--messages
                "read" '(:path "a") "detail"))
              (setq detail
                    (pi-coding-agent--open-tool-detail-buffer "call-1")))
            (pcase cleanup
              ('q
               (with-current-buffer detail
                 (cl-letf (((symbol-function 'quit-window)
                            (lambda (&optional kill _window)
                              (when kill (kill-buffer (current-buffer))))))
                   (pi-coding-agent--quit-tool-detail-buffer))))
              ('abort
               (with-current-buffer chat
                 (pi-coding-agent--set-aborted t)
                 (pi-coding-agent--display-agent-end)))
              ('retry
               (with-current-buffer chat
                 (pi-coding-agent--display-retry-start
                  '(:attempt 1 :maxAttempts 3 :delayMs 10))))
              ('chat-kill (kill-buffer chat)))
            (should-not (buffer-live-p detail))
            (when (buffer-live-p chat)
              (with-current-buffer chat
                (should (= 0 (hash-table-count
                              pi-coding-agent--tool-detail-buffers))))))
        (dolist (buffer (list detail chat))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest pi-coding-agent-test-history-cold-summary-never-renders-full-detail ()
  "Cold replay creates summary metadata without full renderers or buttons."
  (let ((payload (make-string (* 128 1024) ?r))
        (full-render-called nil))
    (with-temp-buffer
      (pi-coding-agent-chat-mode)
      (let ((pi-coding-agent-hot-tail-turn-count 0))
        (cl-letf (((symbol-function 'pi-coding-agent--render-tool-detail-inline)
                   (lambda (&rest _) (setq full-render-called t))))
          (pi-coding-agent--display-session-history
           (pi-coding-agent-summary-test--messages
            "read" '(:path "large.txt") payload)
           (current-buffer)))
        (should-not full-render-called)
        (should (< (buffer-size) 2000))
        (should-not (next-button (point-min)))
        (goto-char (point-min))
        (search-forward "read large.txt")
        (should (pi-coding-agent--cold-tool-block-at-point))))))

(ert-deftest pi-coding-agent-test-summary-path-skips-full-payload-formatters ()
  "Summary construction never invokes full-payload render helpers."
  (let ((payload (make-string (* 128 1024) ?x)))
    (cl-letf (((symbol-function 'ansi-color-filter-apply)
               (lambda (&rest _) (ert-fail "ANSI formatter called")))
              ((symbol-function 'pi-coding-agent--pretty-print-json)
               (lambda (&rest _) (ert-fail "JSON formatter called")))
              ((symbol-function 'pi-coding-agent--insert-rendered-tool-content)
               (lambda (&rest _) (ert-fail "Fence renderer called")))
              ((symbol-function 'pi-coding-agent--apply-diff-overlays)
               (lambda (&rest _) (ert-fail "Diff renderer called"))))
      (dolist (record
               (list
                (pi-coding-agent--tool-summary-record
                 "edit" "edit" '(:path "a")
                 [(:type "text" :text "done")] (list :diff payload) nil)
                (pi-coding-agent--tool-summary-record
                 "read" "read" '(:path "a")
                 (vector (list :type "text" :text payload)) nil nil)
                (pi-coding-agent--tool-summary-record
                 "generic" "custom" (list :payload payload)
                 [(:type "text" :text "done")] (list :data payload) nil)))
        (should (stringp (plist-get record :summary)))))))

(ert-deftest pi-coding-agent-test-cold-tab-opens-detail-and-rebuild-cleans-it ()
  "Cold TAB opens one detail buffer and history rebuild releases it."
  (let ((detail-buffer nil))
    (with-temp-buffer
      (pi-coding-agent-chat-mode)
      (pi-coding-agent--set-chat-session-identity "/tmp/cold/")
      (let ((pi-coding-agent-hot-tail-turn-count 0))
        (pi-coding-agent--display-session-history
         (pi-coding-agent-summary-test--messages
          "read" '(:path "large.txt") (make-string (* 128 1024) ?d))
         (current-buffer)))
      (goto-char (point-min))
      (search-forward "read large.txt")
      (should-not (next-button (point-min)))
      (should-not (seq-some
                   (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                   (overlays-in (point-min) (point-max))))
      (pi-coding-agent-toggle-tool-section)
      (setq detail-buffer
            (gethash "call-1" pi-coding-agent--tool-detail-buffers))
      (should (buffer-live-p detail-buffer))
      (should (= 1 (hash-table-count pi-coding-agent--tool-detail-buffers)))
      (pi-coding-agent--display-session-history
       (pi-coding-agent-summary-test--messages
        "read" '(:path "small.txt") "small")
       (current-buffer))
      (should-not (buffer-live-p detail-buffer))
      (should (= 0 (hash-table-count pi-coding-agent--tool-detail-buffers))))))

(ert-deftest pi-coding-agent-test-hot-and-cold-read-summary-use-offset-target ()
  "Hot and cold read summaries navigate to the authoritative offset."
  (dolist (hot-tail '(1 0))
    (with-temp-buffer
      (pi-coding-agent-chat-mode)
      (pi-coding-agent--set-chat-session-identity "/tmp/nav/")
      (let ((pi-coding-agent-hot-tail-turn-count hot-tail))
        (pi-coding-agent--display-session-history
         (pi-coding-agent-summary-test--messages
          "read" '(:path "src/a.el" :offset 20) "line20\nline21")
         (current-buffer)))
      (goto-char (point-min))
      (search-forward "read src/a.el")
      (let ((target (pi-coding-agent--file-target-at-point)))
        (should (equal "/tmp/nav/src/a.el" (plist-get target :emacs-path)))
        (should (= 20 (plist-get target :line)))))))

(provide 'pi-coding-agent-summary-test)
;;; pi-coding-agent-summary-test.el ends here
