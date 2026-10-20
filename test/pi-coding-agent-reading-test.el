;;; pi-coding-agent-reading-test.el --- Reading contracts -*- lexical-binding: t; -*-
;;; Commentary:
;; Exercise history/live equivalence and independent window behavior.
;;; Code:
(require 'pi-coding-agent-test-common)
(require 'pi-coding-agent-tool-activity-test)

(defmacro pi-coding-agent-reading-test--chat (&rest body)
  "Run BODY in a chat with grouped thinking."
  (declare (indent 0))
  `(pi-coding-agent-tool-activity-test--with-chat
     (let ((pi-coding-agent-thinking-activity-display 'grouped))
       (setq-local pi-coding-agent--thinking-display 'hidden)
       ,@body)))

(ert-deftest pi-coding-agent-test-reading-grouped-history ()
  (pi-coding-agent-reading-test--chat
    (let ((messages
           [(:role "user" :content "explain")
            (:role "assistant" :content
                   [(:type "thinking" :thinking "first secret\nsecond line")
                    (:type "text" :text "progress")
                    (:type "thinking" :thinking "second secret")])
            (:role "assistant" :content
                   [(:type "text" :text "final answer")
                    (:type "text" :text "\ncontinued")])]))
      (pi-coding-agent--display-session-history messages (current-buffer))
      (should (equal messages pi-coding-agent--canonical-messages))
      (should (string-match-p "2 thoughts" (buffer-string)))
      (should-not (string-match-p "secret" (buffer-string)))
      (should (string-match-p "progress" (buffer-string)))
      (let* ((turn pi-coding-agent--reading-current)
             (answer (pi-coding-agent--reading-turn-answer turn)))
        (goto-char answer)
        (should (looking-at "final answer"))
        (should (equal "答复\n"
                       (substring-no-properties
                        (overlay-get (pi-coding-agent--reading-turn-separator turn)
                                     'before-string))))
        (should-not (string-match-p "答复" (buffer-string)))
        (goto-char (pi-coding-agent--reading-turn-start turn))
        (should-not (button-get (button-at (point)) 'face))
        (pi-coding-agent-toggle-tool-section)
        (should (string-match-p "first secret" (buffer-string)))
        (should-not (string-match-p "second line" (buffer-string)))
        (forward-line 1)
        (pi-coding-agent-toggle-tool-section)
        (should (string-match-p "second line" (buffer-string)))
        (goto-char answer)
        (should (looking-at "final answer"))))))

(ert-deftest pi-coding-agent-test-reading-live-stable-retry ()
  (pi-coding-agent-reading-test--chat
    (pi-coding-agent--display-user-message "request")
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    (pi-coding-agent--display-thinking-delta "one")
    (let ((working (buffer-string)))
      (pi-coding-agent--display-thinking-delta " two")
      (pi-coding-agent--display-thinking-end "")
      (pi-coding-agent--display-thinking-start)
      (pi-coding-agent--display-thinking-end "three")
      (should (equal working (buffer-string))))
    (pi-coding-agent--reading-message-start)
    (pi-coding-agent--display-message-delta "answer")
    (pi-coding-agent--display-agent-end)
    (should (string-match-p "2 thoughts" (buffer-string)))
    (let ((turn pi-coding-agent--reading-current))
      (pi-coding-agent--reading-run-start)
      (should (eq turn pi-coding-agent--reading-current))
      (should-not (pi-coding-agent--reading-turn-answer turn))
      (should (string-match-p "working" (buffer-string))))))

(ert-deftest pi-coding-agent-test-reading-empty-and-visible ()
  (pi-coding-agent-reading-test--chat
    (pi-coding-agent--display-session-history
     [(:role "user" :content "request")
      (:role "assistant" :content [(:type "thinking" :thinking " \n")])]
     (current-buffer))
    (should-not (pi-coding-agent--reading-turn-start pi-coding-agent--reading-current))
    (setq-local pi-coding-agent--thinking-display 'visible)
    (pi-coding-agent--display-session-history
     [(:role "user" :content "request")
      (:role "assistant" :content [(:type "thinking" :thinking "full thought")])]
     (current-buffer))
    (should (string-match-p "full thought" (buffer-string)))
    (should-not (string-match-p "1 thoughts" (buffer-string)))))

(ert-deftest pi-coding-agent-test-reading-success-evidence ()
  (pi-coding-agent-reading-test--chat
    (let ((messages (pi-coding-agent-tool-activity-test--messages
                     '(("read" (:path "a.el") "read" nil nil)
                       ("edit" (:path "./a.el") "ok" (:diff "+new\n-old") nil)
                       ("edit" (:path "a.el") "ok" (:diff "+again") nil)
                       ("write" (:path "failed.el" :content "bad") "error" nil t)
                       ("bash" (:command "echo a.el") "a.el" nil nil)))))
      (pi-coding-agent--display-session-history messages (current-buffer))
      (let* ((record (gethash 0 pi-coding-agent--activity-table))
             (counts (pi-coding-agent--activity-aggregates record))
             (summary (string-join (pi-coding-agent--activity-summary-parts record) " · ")))
        (should (= 1 (plist-get counts :files)))
        (should (= 1 (plist-get counts :read)))
        (should (= 2 (plist-get counts :added)))
        (should (= 1 (plist-get counts :removed)))
        (should (string-match-p
                 (regexp-quote "1 call failed · edited 1 · ops +2 -1 · read 1")
                 summary))))))

(ert-deftest pi-coding-agent-test-reading-bounded-error-payload ()
  (pi-coding-agent-reading-test--chat
    (let ((payload (concat "Build report\nError: missing dependency\n"
                           (make-string (* 1024 1024) ?x))))
      (cl-letf (((symbol-function 'pi-coding-agent--tool-result-text)
                 (lambda (&rest _) (ert-fail "full payload formatted"))))
        (let ((record (pi-coding-agent--activity-completed-tool-record
                       "large" "bash" '(:command "build")
                       (vector (list :type "text" :text payload)) nil t)))
          (should (string-match-p "Error: missing dependency" (plist-get record :summary)))
          (should-not (string-match-p "Build report" (plist-get record :summary)))
          (should-not (plist-get record :line-count)))))))

(ert-deftest pi-coding-agent-test-reading-window-independent ()
  (save-window-excursion
    (pi-coding-agent-reading-test--chat
      (switch-to-buffer (current-buffer))
      (let ((inhibit-read-only t)) (insert (make-string 300 ?x)))
      (let ((tail (selected-window))
            (history (split-window-below)))
        (set-window-point tail (point-max))
        (set-window-point history 1)
        (pi-coding-agent--reading-content-event)
        (should-not (pi-coding-agent-reading-status tail))
        (should (equal "阅读历史 · 有新输出" (pi-coding-agent-reading-status history)))
        (set-window-point history (point-max))
        (pi-coding-agent--viewport-window-scrolled
         history (window-start history))
        (should-not (pi-coding-agent-reading-status history))))))

(ert-deftest pi-coding-agent-test-reading-status-is-read-only ()
  (save-window-excursion
    (pi-coding-agent-reading-test--chat
      (switch-to-buffer (current-buffer))
      (let ((inhibit-read-only t))
        (insert (make-string 300 ?x)))
      (let ((window (selected-window)))
        (set-window-point window 1)
        (pi-coding-agent--viewport-reconcile-window window)
        (pi-coding-agent--reading-content-event)
        (let ((before (pi-coding-agent-viewport-state window)))
          (should (equal "阅读历史 · 有新输出"
                         (pi-coding-agent-reading-status window)))
          (should (equal before
                         (pi-coding-agent-viewport-state window))))))))

(ert-deftest pi-coding-agent-test-reading-status-does-not-initialize-state ()
  (save-window-excursion
    (pi-coding-agent-reading-test--chat
      (switch-to-buffer (current-buffer))
      (let ((window (selected-window)))
        (set-window-parameter
         window pi-coding-agent--viewport-state-parameter nil)
        (should-not
         (window-parameter
          window pi-coding-agent--viewport-state-parameter))
        (should-not (pi-coding-agent-reading-status window))
        (should-not (pi-coding-agent-viewport-state window))
        (should-not
         (window-parameter
          window pi-coding-agent--viewport-state-parameter))))))

(ert-deftest pi-coding-agent-test-reading-title-skill-line ()
  (should (equal "修复中文标题"
                 (pi-coding-agent--reading-first-request "/skill inspect\n\n修复中文标题\nmore"))))

(ert-deftest pi-coding-agent-test-reading-navigation-from-input ()
  (save-window-excursion
    (pi-coding-agent-reading-test--chat
      (let ((chat (current-buffer))
            (input (generate-new-buffer " *pi-reading-input*")))
        (unwind-protect
            (progn
              (switch-to-buffer chat)
              (let ((output-window (selected-window))
                    (input-window (split-window-below)))
                (pi-coding-agent--display-session-history
                 [(:role "user" :content "one")
                  (:role "assistant" :content [(:type "text" :text "answer one")])
                  (:role "user" :content "two")
                  (:role "assistant" :content [(:type "text" :text "answer two")])]
                 chat)
                (set-window-buffer input-window input)
                (with-current-buffer input
                  (pi-coding-agent-input-mode)
                  (insert "unfinished draft"))
                (select-window input-window)
                (cl-letf (((symbol-function 'pi-coding-agent--get-chat-buffer)
                           (lambda () chat))
                          ((symbol-function 'message) #'ignore))
                  (pi-coding-agent-goto-latest-answer))
                (should (eq (selected-window) input-window))
                (should (equal "unfinished draft" (buffer-string)))
                (with-current-buffer chat
                  (save-excursion
                    (goto-char (window-point output-window))
                    (should (looking-at "answer two"))))))
          (kill-buffer input))))))

(ert-deftest pi-coding-agent-test-reading-failure-navigation-boundaries ()
  (save-window-excursion
    (pi-coding-agent-reading-test--chat
      (switch-to-buffer (current-buffer))
      (pi-coding-agent--display-session-history
       (pi-coding-agent-tool-activity-test--messages
        '(("bash" (:command "false") "error one" nil t))
        '(("bash" (:command "false") "error two" nil t)))
       (current-buffer))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'message) #'ignore))
        (pi-coding-agent-next-failed-tool)
        (should (equal "call-1"
                       (get-text-property (point) 'pi-coding-agent-activity-tool-call-id)))
        (pi-coding-agent-next-failed-tool)
        (should (equal "call-2"
                       (get-text-property (point) 'pi-coding-agent-activity-tool-call-id)))
        (let ((end (point)))
          (pi-coding-agent-next-failed-tool)
          (should (= end (point))))
        (pi-coding-agent-previous-failed-tool)
        (should (equal "call-1"
                       (get-text-property (point) 'pi-coding-agent-activity-tool-call-id)))))))

(ert-deftest pi-coding-agent-test-reading-500-turns-bounded-projection ()
  (pi-coding-agent-reading-test--chat
    (let (messages)
      (dotimes (turn 500)
        (push (list :role "user" :content (format "turn %d" turn)) messages)
        (push '(:role "assistant" :content
                       [(:type "thinking" :thinking "one")
                        (:type "thinking" :thinking "two")
                        (:type "thinking" :thinking "three")
                        (:type "text" :text "answer")]) messages))
      (pi-coding-agent--display-session-history
       (vconcat (nreverse messages)) (current-buffer))
      (should (= 500 (length pi-coding-agent--reading-turns)))
      (should (= 500 (how-many "3 thoughts" (point-min) (point-max))))
      (should (< (buffer-size) 45000))
      (let ((turn (car pi-coding-agent--reading-turns)))
        (pi-coding-agent--reading-run-start)
        (pi-coding-agent--display-agent-start)
        (pi-coding-agent--display-thinking-start)
        (pi-coding-agent--display-thinking-delta "latest")
        (pi-coding-agent--display-thinking-end nil)
        (should (= 4 (length (pi-coding-agent--reading-turn-thoughts turn))))))))

(ert-deftest pi-coding-agent-test-reading-live-fragment-expansion ()
  (pi-coding-agent-reading-test--chat
    (pi-coding-agent--display-user-message "request")
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    (pi-coding-agent--display-thinking-delta "first\nhidden")
    (goto-char (pi-coding-agent--reading-turn-start pi-coding-agent--reading-current))
    (pi-coding-agent-toggle-tool-section)
    (forward-line 1)
    (pi-coding-agent-toggle-tool-section)
    (should (string-match-p "hidden" (buffer-string)))
    (pi-coding-agent--display-thinking-delta " continuation")
    (should (string-match-p "hidden continuation" (buffer-string)))
    (pi-coding-agent--display-thinking-end nil)
    (pi-coding-agent--display-agent-end)
    (should (string-match-p "hidden continuation" (buffer-string)))))

(ert-deftest pi-coding-agent-test-reading-visible-toggle-roundtrip ()
  (pi-coding-agent-reading-test--chat
    (pi-coding-agent--display-session-history
     [(:role "user" :content "request")
      (:role "assistant" :content [(:type "thinking" :thinking "secret one")
                                  (:type "text" :text "middle")
                                  (:type "thinking" :thinking "secret two")])]
     (current-buffer))
    (pi-coding-agent--set-chat-thinking-display 'visible)
    (should (string-match-p "secret one" (buffer-string)))
    (should (< (string-match "middle" (buffer-string))
               (string-match "secret two" (buffer-string))))
    (pi-coding-agent--set-chat-thinking-display 'hidden)
    (should (string-match-p "2 thoughts" (buffer-string)))
    (should-not (string-match-p "secret" (buffer-string)))))

(ert-deftest pi-coding-agent-test-reading-narrow-test-failure ()
  (pi-coding-agent-reading-test--chat
    (pi-coding-agent--display-session-history
     (pi-coding-agent-tool-activity-test--messages
      '(("bash" (:command "test") "report" (:testStatus "failed") nil)))
     (current-buffer))
    (let ((record (gethash 0 pi-coding-agent--activity-table)))
      (setf (pi-coding-agent--activity-record-test-failed record) 1)
      (dolist (style '(emoji text none))
        (let ((pi-coding-agent-activity-icon-style style))
          (cl-letf (((symbol-function 'pi-coding-agent--activity-line-budget)
                     (lambda () 1)))
            (let ((summary (pi-coding-agent--activity-group-text record)))
              (should (equal summary "!"))
              (should (string-match-p "tests failed"
                                      (get-text-property 0 'help-echo summary))))))))))

(ert-deftest pi-coding-agent-test-reading-custom-welcome ()
  (pi-coding-agent-reading-test--chat
    (pi-coding-agent--display-session-history
     [(:role "custom" :display t :content "custom welcome content")]
     (current-buffer))
    (should (string-match-p "custom welcome content" (buffer-string)))
    (should-not (string-match-p
                 (regexp-quote (pi-coding-agent--format-startup-header))
                 (buffer-string)))))

(ert-deftest pi-coding-agent-test-reading-answer-after-deferred-tools ()
  (pi-coding-agent-reading-test--chat
    (let ((messages
           (vconcat
            (pi-coding-agent-tool-activity-test--messages
             '(("read" (:path "a.el") "contents" nil nil)))
            [(:role "assistant" :content [(:type "text" :text "final answer")])])))
      (pi-coding-agent--display-session-history messages (current-buffer))
      (let ((answer (pi-coding-agent--reading-turn-answer pi-coding-agent--reading-current)))
        (goto-char answer)
        (should (looking-at "final answer"))
        (let ((record (gethash 0 pi-coding-agent--activity-table)))
          (pi-coding-agent--reading-expand-tools record)
          (pi-coding-agent--activity-render-record record))
        (goto-char answer)
        (should (looking-at "final answer"))))))

(ert-deftest pi-coding-agent-test-reading-global-window-hook-takes-frame ()
  (save-window-excursion
    (let ((first (generate-new-buffer " *pi-reading-first*"))
          (second (generate-new-buffer " *pi-reading-second*"))
          (window (selected-window))
          state point-marker second-state)
      (unwind-protect
          (progn
            (set-window-buffer window first)
            (with-current-buffer first
              (pi-coding-agent-chat-mode)
              (setq-local pi-coding-agent--viewport-content-generation 7)
              (let ((inhibit-read-only t))
                (insert "history\n"))
              (set-window-point window (point-min))
              (setq state (pi-coding-agent--viewport-window-state window))
              (pi-coding-agent--viewport-enter-reading state t)
              (setq point-marker
                    (pi-coding-agent--viewport-state-point-marker state)))
            (pi-coding-agent--viewport-window-rebound (selected-frame))
            (should (eq state
                        (window-parameter
                         window pi-coding-agent--viewport-state-parameter)))
            (should (marker-position point-marker))
            (set-window-buffer window second)
            (run-hook-with-args 'window-buffer-change-functions (selected-frame))
            (should-not
             (window-parameter
              window pi-coding-agent--viewport-state-parameter))
            (should-not (marker-position point-marker))
            (with-current-buffer second
              (pi-coding-agent-chat-mode)
              ;; A matching numeric generation cannot preserve the old chat.
              (setq-local pi-coding-agent--viewport-content-generation 7)
              (setq second-state
                    (pi-coding-agent--viewport-window-state window)))
            (should-not (eq state second-state))
            (should (eq (pi-coding-agent--viewport-state-chat second-state)
                        second))
            (should (= (pi-coding-agent--viewport-state-content-generation
                        second-state)
                       7)))
        (kill-buffer first)
        (kill-buffer second)))))

(ert-deftest pi-coding-agent-test-resume-following-settles-output-from-input ()
  "Resume uses shared tail settlement while preserving Input focus and draft."
  (save-window-excursion
    (delete-other-windows)
    (let ((chat (generate-new-buffer " *pi-resume-output*"))
          (input (generate-new-buffer " *pi-resume-input*"))
          settled)
      (unwind-protect
          (let* ((output-window (selected-window))
                 (input-window (split-window output-window -5 'below)))
            (set-window-buffer output-window chat)
            (set-window-buffer input-window input)
            (with-current-buffer chat
              (pi-coding-agent-chat-mode)
              (setq-local pi-coding-agent--input-buffer input)
              (let ((inhibit-read-only t))
                (dotimes (index 40)
                  (insert (format "line %d\n" index)))))
            (with-current-buffer input
              (pi-coding-agent-input-mode)
              (setq-local pi-coding-agent--chat-buffer chat)
              (insert "keep this draft"))
            (select-window input-window)
            (let ((original
                   (symbol-function
                    'pi-coding-agent--settle-following-window)))
              (cl-letf (((symbol-function
                          'pi-coding-agent--settle-following-window)
                         (lambda (window)
                           (setq settled window)
                           (funcall original window))))
                (with-current-buffer input
                  (pi-coding-agent-resume-following))))
            (should (eq settled output-window))
            (should (eq (selected-window) input-window))
            (should
             (equal (with-current-buffer input (buffer-string))
                    "keep this draft"))
            (let ((state
                   (with-current-buffer chat
                     (pi-coding-agent-viewport-state output-window))))
              (should (plist-get state :following))
              (should-not (plist-get state :new-output))))
        (when (buffer-live-p chat)
          (kill-buffer chat))
        (when (buffer-live-p input)
          (kill-buffer input))))))

(provide 'pi-coding-agent-reading-test)
;;; pi-coding-agent-reading-test.el ends here
