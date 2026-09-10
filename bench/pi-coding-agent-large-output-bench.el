;;; pi-coding-agent-large-output-bench.el --- Large output benchmarks -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic history and streaming workloads.  No user session files are read.

;;; Code:

(require 'benchmark)
(require 'pi-coding-agent)

(defconst pi-coding-agent-large-output-bench--sizes
  (list (* 500 1024) (* 1024 1024) (* 5 1024 1024)))

(defconst pi-coding-agent-large-output-bench--delta-sizes '(128 1024 8192))

(defconst pi-coding-agent-large-output-bench--parser-counts
  '(2 100 500 1000)
  "Approximate Markdown inline parser counts used by lifecycle benchmarks.")

(defconst pi-coding-agent-large-output-bench--parser-payload-size
  (* 128 1024)
  "Target byte size for every parser-cardinality workload.")

(defconst pi-coding-agent-large-output-bench--structured-fixture
  (concat
   "Plain paragraph with **bold**, `inline code`, and [a link](file.el:12).\n\n"
   "> Thinking-style quoted text with *emphasis*.\n\n"
   "```elisp\n(message \"synthetic\")\n```\n\n"
   "Tool-like output: read file.el\n\n"
   "![synthetic image](file:///tmp/nonexistent.png)\n\n"
   "| Kind | Value |\n| --- | ---: |\n| parser | 1000 |\n")
  "Synthetic mixed Markdown fixture; never reads user session data.")

(defun pi-coding-agent-large-output-bench--result
    (kind size delta elapsed buffer-size)
  "Print one benchmark result for KIND, SIZE, DELTA, ELAPSED and BUFFER-SIZE."
  (prin1
   (list :kind kind
         :bytes size
         :delta-bytes delta
         :seconds (nth 0 elapsed)
         :gc-count (nth 1 elapsed)
         :gc-seconds (nth 2 elapsed)
         :buffer-bytes buffer-size))
  (terpri))

(defun pi-coding-agent-large-output-bench--result-plist
    (kind size delta elapsed buffer-size)
  "Return one result plist for KIND, SIZE, DELTA, ELAPSED and BUFFER-SIZE."
  (list :kind kind
        :bytes size
        :delta-bytes delta
        :seconds (nth 0 elapsed)
        :gc-count (nth 1 elapsed)
        :gc-seconds (nth 2 elapsed)
        :buffer-bytes buffer-size))

(defun pi-coding-agent-large-output-bench--payload (size)
  "Return deterministic Markdown text of approximately SIZE bytes."
  (let ((line "Synthetic output line with **markdown** and `code`.\n"))
    (with-temp-buffer
      (while (< (buffer-size) size)
        (insert line))
      (buffer-substring-no-properties (point-min) (1+ size)))))

(defun pi-coding-agent-large-output-bench--parser-payload
    (parser-count size)
  "Return SIZE bytes yielding approximately PARSER-COUNT inline parsers.
`md-ts-mode' keeps one placeholder inline parser, so the payload contains one
fewer paragraph than PARSER-COUNT."
  (let* ((paragraphs (max 1 (1- parser-count)))
         (bases
          (cl-loop for index below paragraphs
                   collect
                   (format "Paragraph %04d with **bold** and `code`." index)))
         (base-size
          (+ (apply #'+ (mapcar #'length bases))
             (* 2 (1- paragraphs))))
         (padding (max 0 (- size base-size)))
         (padding-each (/ padding paragraphs))
         (padding-remainder (% padding paragraphs)))
    (with-temp-buffer
      (cl-loop
       for text in bases
       for index from 0
       do
        (unless (bobp)
          (insert "\n\n"))
        (insert text)
        (insert
         (make-string
          (+ padding-each (if (< index padding-remainder) 1 0))
          ?x)))
      (buffer-substring-no-properties
       (point-min) (min (point-max) (1+ size))))))

(defun pi-coding-agent-large-output-bench--inline-parser-count ()
  "Return the number of Markdown inline parsers in the current buffer."
  (cl-count 'markdown-inline
            (condition-case nil
                (treesit-parser-list nil nil t)
              (wrong-number-of-arguments
               (treesit-parser-list)))
            :key #'treesit-parser-language))

(defun pi-coding-agent-large-output-bench--parser-overlay-count ()
  "Return the number of local parser ownership overlays in this buffer."
  (cl-count-if
   (lambda (overlay)
     (and (overlay-get overlay 'treesit-parser)
          (overlay-get overlay 'treesit-host-parser)
          (overlay-get overlay 'treesit-parser-ov-timestamp)))
   (overlays-in (point-min) (point-max))))

(defun pi-coding-agent-large-output-bench--run-parser-workload
    (parser-count visible redisplay-p &optional lifecycle iterations)
  "Run one parser workload for PARSER-COUNT.
VISIBLE controls whether the benchmark buffer is displayed.  REDISPLAY-P
forces redisplay after each incremental update when VISIBLE is non-nil.
LIFECYCLE enables cold parser reclamation before incremental updates.
ITERATIONS defaults to 200."
  (let ((buffer (generate-new-buffer " *pi-parser-lifecycle-bench*"))
        (payload
         (pi-coding-agent-large-output-bench--parser-payload
          parser-count pi-coding-agent-large-output-bench--parser-payload-size))
        (updates 0)
        (redisplays 0)
        elapsed
        metrics)
    (setq iterations (or iterations 200))
    (unwind-protect
        (save-window-excursion
          (when visible
            (switch-to-buffer buffer))
          (with-current-buffer buffer
            (pi-coding-agent-chat-mode)
            (setq-local pi-coding-agent-parser-lifecycle-enabled lifecycle)
            (let ((inhibit-read-only t))
              (insert payload))
            (font-lock-ensure (point-min) (point-max))
            (pi-coding-agent--cancel-parser-reconciliation)
            (when visible
              (redisplay t))
            (let ((before-parsers
                   (pi-coding-agent-large-output-bench--inline-parser-count)))
              (move-marker pi-coding-agent--hot-tail-start (point-max))
              (when lifecycle
                (pi-coding-agent--reconcile-local-parsers))
            (goto-char (point-max))
            (pi-coding-agent--display-agent-start)
            (add-hook 'after-change-functions
                      (lambda (&rest _args) (setq updates (1+ updates)))
                      nil t)
            (garbage-collect)
            (setq elapsed
                  (benchmark-run
                    1
                    (dotimes (_ iterations)
                      (pi-coding-agent--display-message-delta " streamed")
                      (when (and visible redisplay-p)
                        (setq redisplays (1+ redisplays))
                        (redisplay t)))))
            (setq metrics
                  (list
                   :kind 'parser-cardinality
                   :requested-parsers parser-count
                   :parser-lifecycle lifecycle
                   :iterations iterations
                   :before-inline-parsers before-parsers
                   :inline-parsers
                   (pi-coding-agent-large-output-bench--inline-parser-count)
                   :ownership-overlays
                   (pi-coding-agent-large-output-bench--parser-overlay-count)
                   :visible visible
                   :redisplay redisplay-p
                   :buffer-updates updates
                   :redisplays redisplays
                   :seconds (nth 0 elapsed)
                   :gc-count (nth 1 elapsed)
                   :gc-seconds (nth 2 elapsed)
                   :buffer-bytes (buffer-size))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    metrics))

;;;###autoload
(defun pi-coding-agent-large-output-bench-run-parser-lifecycle ()
  "Run the parser-cardinality benchmark matrix and print result plists."
  (interactive)
  (let ((visible-options
         (if (display-graphic-p) '(nil t) '(nil))))
    (dolist (parser-count pi-coding-agent-large-output-bench--parser-counts)
      (dolist (visible visible-options)
        (dolist (redisplay-p (if visible '(nil t) '(nil)))
          (dolist (lifecycle '(nil t))
            (prin1
             (pi-coding-agent-large-output-bench--run-parser-workload
              parser-count visible redisplay-p lifecycle
              (if (and visible redisplay-p) 20 200)))
            (terpri)))))))

(defun pi-coding-agent-large-output-bench--cross-read-stream
    (batching count)
  "Benchmark COUNT one-delta reads with cross-read BATCHING."
  (let ((buffer (generate-new-buffer " *pi-cross-read-bench*"))
        (process (start-process "cat" nil "cat"))
        (updates 0)
        elapsed
        metrics)
    (unwind-protect
        (with-current-buffer buffer
          (pi-coding-agent-chat-mode)
          (setq-local pi-coding-agent-cross-read-batching-enabled batching
                      pi-coding-agent-cross-read-batching-delay 10
                      pi-coding-agent--session-transition-generation 0
                      pi-coding-agent--process process)
          (set-process-buffer process buffer)
          (process-put process 'pi-coding-agent-chat-buffer buffer)
          (pi-coding-agent--register-display-handler process)
          (add-hook 'after-change-functions
                    (lambda (&rest _arguments)
                      (setq updates (1+ updates)))
                    nil t)
          (pi-coding-agent--process-filter
           process
           (concat
            (json-encode '(:type "agent_start"))
            "\n"
            (json-encode
             '(:type "message_start"
               :message (:role "assistant" :timestamp 1)))
            "\n"))
          (garbage-collect)
          (setq elapsed
                (benchmark-run
                  1
                  (dotimes (_ count)
                    (pi-coding-agent--process-filter
                     process
                     (concat
                      (json-encode
                       '(:type "message_update"
                         :message (:role "assistant" :timestamp 1)
                         :assistantMessageEvent
                         (:type "text_delta"
                          :contentIndex 0
                          :delta "x")))
                      "\n")))
                  (pi-coding-agent--flush-pending-delta process 'benchmark)))
          (setq metrics
                (list :kind 'cross-read
                      :batching batching
                      :delta-count count
                      :buffer-updates updates
                      :seconds (nth 0 elapsed)
                      :gc-count (nth 1 elapsed)
                      :gc-seconds (nth 2 elapsed)
                      :buffer-bytes (buffer-size))))
      (pi-coding-agent--cancel-pending-delta process)
      (when (processp process)
        (pi-coding-agent--unregister-display-handler process)
        (when (process-live-p process)
          (delete-process process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq pi-coding-agent--process nil)
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    metrics))

;;;###autoload
(defun pi-coding-agent-large-output-bench-run-cross-read ()
  "Run cross-read batching A/B workloads and print result plists."
  (interactive)
  (dolist (batching '(nil t))
    (prin1
     (pi-coding-agent-large-output-bench--cross-read-stream batching 500))
    (terpri)))

(defun pi-coding-agent-large-output-bench--combined-gui
    (parser-count lifecycle batching delta-count)
  "Measure visible streaming with parser and BATCHING controls.
PARSER-COUNT sets initial cardinality, LIFECYCLE controls cold reclamation,
and DELTA-COUNT is the number of one-character process reads."
  (let ((buffer (generate-new-buffer " *pi-parser-combined-gui-bench*"))
        (process (start-process "cat" nil "cat"))
        (payload
         (pi-coding-agent-large-output-bench--parser-payload
          parser-count pi-coding-agent-large-output-bench--parser-payload-size))
        (updates 0)
        elapsed
        metrics)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (with-current-buffer buffer
            (pi-coding-agent-chat-mode)
            (setq-local pi-coding-agent-parser-lifecycle-enabled lifecycle
                        pi-coding-agent-parser-reconcile-delay 999
                        pi-coding-agent-cross-read-batching-enabled batching
                        pi-coding-agent-cross-read-batching-delay 10
                        pi-coding-agent--session-transition-generation 0
                        pi-coding-agent--process process)
            (let ((inhibit-read-only t))
              (insert payload))
            (font-lock-ensure)
            (pi-coding-agent--cancel-parser-reconciliation)
            (move-marker pi-coding-agent--hot-tail-start (point-max))
            (when lifecycle
              (pi-coding-agent--reconcile-local-parsers))
            (set-process-buffer process buffer)
            (set-process-query-on-exit-flag process nil)
            (process-put process 'pi-coding-agent-chat-buffer buffer)
            (pi-coding-agent--register-display-handler process)
            (dolist
                (event
                 (list
                  '(:type "agent_start")
                  '(:type "message_start"
                    :message (:role "assistant" :timestamp 1))))
              (pi-coding-agent--process-filter
               process (concat (json-encode event) "\n")))
            (goto-char (point-max))
            (set-window-point (selected-window) (point-max))
            (redisplay t)
            (add-hook 'after-change-functions
                      (lambda (&rest _arguments)
                        (setq updates (1+ updates)))
                      nil t)
            (garbage-collect)
            (setq elapsed
                  (benchmark-run
                    1
                    (dotimes (_ delta-count)
                      (pi-coding-agent--process-filter
                       process
                       (concat
                        (json-encode
                         '(:type "message_update"
                           :message (:role "assistant" :timestamp 1)
                           :assistantMessageEvent
                           (:type "text_delta"
                            :contentIndex 0
                            :delta "x")))
                        "\n"))
                      (redisplay t))
                    (pi-coding-agent--flush-pending-delta process 'benchmark)
                    (redisplay t)))
            (setq metrics
                  (list
                   :kind 'combined-gui
                   :requested-parsers parser-count
                   :parser-lifecycle lifecycle
                   :batching batching
                   :delta-count delta-count
                   :inline-parsers
                   (pi-coding-agent-large-output-bench--inline-parser-count)
                   :ownership-overlays
                   (pi-coding-agent-large-output-bench--parser-overlay-count)
                   :buffer-updates updates
                   :seconds (nth 0 elapsed)
                   :gc-count (nth 1 elapsed)
                   :gc-seconds (nth 2 elapsed)
                   :buffer-bytes (buffer-size)))))
      (pi-coding-agent--cancel-pending-delta process)
      (when (processp process)
        (pi-coding-agent--unregister-display-handler process)
        (when (process-live-p process)
          (delete-process process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (pi-coding-agent--cleanup-parser-lifecycle)
          (setq pi-coding-agent--process nil)
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    metrics))

;;;###autoload
(defun pi-coding-agent-large-output-bench-run-combined-gui ()
  "Run the four-way visible parser lifecycle and batching comparison."
  (interactive)
  (unless (display-graphic-p)
    (user-error "Combined benchmark requires a graphical frame"))
  (let (results)
    (dolist (lifecycle '(nil t))
      (dolist (batching '(nil t))
        (push
         (pi-coding-agent-large-output-bench--combined-gui
          1000 lifecycle batching 50)
         results)))
    (nreverse results)))

(defun pi-coding-agent-large-output-bench--history (size)
  "Benchmark rendering a synthetic assistant history of SIZE bytes."
  (let ((payload (pi-coding-agent-large-output-bench--payload size))
        elapsed
        rendered-size)
    (with-temp-buffer
      (pi-coding-agent-chat-mode)
      (garbage-collect)
      (setq elapsed
            (benchmark-run
              1
              (pi-coding-agent--display-session-history
               (vector
                (list :role "assistant"
                      :content (vector (list :type "text" :text payload))))
               (current-buffer))))
      (setq rendered-size (buffer-size)))
    (pi-coding-agent-large-output-bench--result
     'history size nil elapsed rendered-size)))

(defun pi-coding-agent-large-output-bench--button-count ()
  "Return the number of buttons in the current buffer."
  (let ((position (point-min))
        (count 0)
        button)
    (while (setq button (next-button position))
      (setq count (1+ count)
            position (max (1+ (button-start button))
                          (button-end button))))
    count))

(defun pi-coding-agent-large-output-bench--tool-history (kind size)
  "Benchmark summary-first history replay for tool KIND and payload SIZE."
  (let* ((payload (pi-coding-agent-large-output-bench--payload size))
         (name (symbol-name kind))
         (args (pcase kind
                 ('read '(:path "/tmp/large.txt" :offset 20))
                 ('edit '(:path "/tmp/large.txt"))
                 ('write (list :path "/tmp/large.txt" :content payload))
                 ('bash '(:command "large-command"))))
         (details (and (eq kind 'edit) (list :diff payload)))
         (messages
          (vector
           (list :role "assistant"
                 :content
                 (vector (list :type "toolCall" :id "tool-1"
                               :name name :arguments args)))
           (list :role "toolResult" :toolCallId "tool-1" :toolName name
                 :content (vector (list :type "text" :text payload))
                 :details details :isError :json-false)))
         elapsed
         metrics)
    (with-temp-buffer
      (pi-coding-agent-chat-mode)
      (garbage-collect)
      (setq elapsed
            (benchmark-run
              1
              (pi-coding-agent--display-session-history
               messages (current-buffer))))
      (setq metrics
            (list :kind (intern (format "tool-%s" name))
                  :bytes size
                  :seconds (nth 0 elapsed)
                  :gc-count (nth 1 elapsed)
                  :gc-seconds (nth 2 elapsed)
                  :buffer-bytes (buffer-size)
                  :overlays (length (overlays-in (point-min) (point-max)))
                  :buttons (pi-coding-agent-large-output-bench--button-count))))
    (prin1 metrics)
    (terpri)
    metrics))

(defun pi-coding-agent-large-output-bench--tool-live (kind size)
  "Benchmark summary-first live completion for tool KIND and payload SIZE."
  (let* ((payload (pi-coding-agent-large-output-bench--payload size))
         (name (symbol-name kind))
         (args (pcase kind
                 ('read '(:path "/tmp/large.txt" :offset 20))
                 ('edit '(:path "/tmp/large.txt"))
                 ('write (list :path "/tmp/large.txt" :content payload))
                 ('bash '(:command "large-command"))))
         (details (and (eq kind 'edit) (list :diff payload)))
         elapsed
         metrics)
    (with-temp-buffer
      (pi-coding-agent-chat-mode)
      (pi-coding-agent--handle-display-event
       (list :type "tool_execution_start" :toolCallId "tool-1"
             :toolName name :args args))
      (garbage-collect)
      (setq elapsed
            (benchmark-run
              1
              (pi-coding-agent--handle-display-event
               (list :type "tool_execution_end" :toolCallId "tool-1"
                     :toolName name
                     :result
                     (list :content (vector (list :type "text" :text payload))
                           :details details)
                     :isError nil))))
      (setq metrics
            (list :kind (intern (format "tool-live-%s" name))
                  :bytes size
                  :seconds (nth 0 elapsed)
                  :gc-count (nth 1 elapsed)
                  :gc-seconds (nth 2 elapsed)
                  :buffer-bytes (buffer-size)
                  :overlays (length (overlays-in (point-min) (point-max)))
                  :buttons (pi-coding-agent-large-output-bench--button-count))))
    (prin1 metrics)
    (terpri)
    metrics))

(defun pi-coding-agent-large-output-bench--stream (kind size delta-size)
  "Benchmark streaming KIND to SIZE bytes in DELTA-SIZE chunks."
  (let ((remaining size)
        (chunk (make-string delta-size ?x))
        elapsed
        rendered-size)
    (with-temp-buffer
      (pi-coding-agent-chat-mode)
      (pi-coding-agent--display-agent-start)
      (when (eq kind 'thinking)
        (pi-coding-agent--display-thinking-start))
      (garbage-collect)
      (setq elapsed
            (benchmark-run
              1
              (while (> remaining 0)
                (let ((delta (if (< remaining delta-size)
                                 (substring chunk 0 remaining)
                               chunk)))
                  (if (eq kind 'thinking)
                      (pi-coding-agent--display-thinking-delta delta)
                    (pi-coding-agent--display-message-delta delta))
                  (setq remaining (- remaining (length delta)))))
              (when (eq kind 'thinking)
                (pi-coding-agent--display-thinking-end ""))))
      (setq rendered-size (buffer-size)))
    (pi-coding-agent-large-output-bench--result
     kind size delta-size elapsed rendered-size)))

;;;###autoload
(defun pi-coding-agent-large-output-bench-run ()
  "Run the synthetic large-output benchmark matrix."
  (dolist (size pi-coding-agent-large-output-bench--sizes)
    (pi-coding-agent-large-output-bench--history size))
  (dolist (kind '(read edit write bash))
    (pi-coding-agent-large-output-bench--tool-history kind (* 1024 1024))
    (pi-coding-agent-large-output-bench--tool-live kind (* 1024 1024)))
  (dolist (size pi-coding-agent-large-output-bench--sizes)
    (dolist (delta-size pi-coding-agent-large-output-bench--delta-sizes)
      (pi-coding-agent-large-output-bench--stream 'text size delta-size)
      (pi-coding-agent-large-output-bench--stream
       'thinking size delta-size))))

(defun pi-coding-agent-large-output-bench--gui-stream (kind size delta-size)
  "Measure visible streaming KIND to SIZE bytes in DELTA-SIZE chunks."
  (let ((buffer (generate-new-buffer " *pi-large-output-gui-bench*"))
        (remaining size)
        (chunk (make-string delta-size ?x))
        elapsed
        rendered-size)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (let ((pi-coding-agent-thinking-display 'visible))
            (pi-coding-agent-chat-mode)
            (pi-coding-agent--display-agent-start)
            (when (eq kind 'thinking)
              (pi-coding-agent--display-thinking-start))
            (garbage-collect)
            (setq elapsed
                  (benchmark-run
                    1
                    (while (> remaining 0)
                      (let ((delta (if (< remaining delta-size)
                                       (substring chunk 0 remaining)
                                     chunk)))
                        (if (eq kind 'thinking)
                            (pi-coding-agent--display-thinking-delta delta)
                          (pi-coding-agent--display-message-delta delta))
                        (setq remaining (- remaining (length delta)))
                        (redisplay t)))
                    (when (eq kind 'thinking)
                      (pi-coding-agent--display-thinking-end ""))
                    (redisplay t))))
          (setq rendered-size (buffer-size)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    (pi-coding-agent-large-output-bench--result-plist
     kind size delta-size elapsed rendered-size)))

(defun pi-coding-agent-large-output-bench--gui-history (size)
  "Measure visible history rendering and redisplay for SIZE bytes."
  (let ((buffer (generate-new-buffer " *pi-large-history-gui-bench*"))
        (payload (pi-coding-agent-large-output-bench--payload size))
        elapsed
        rendered-size)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (pi-coding-agent-chat-mode)
          (garbage-collect)
          (setq elapsed
                (benchmark-run
                  1
                  (pi-coding-agent--display-session-history
                   (vector
                    (list :role "assistant"
                          :content
                          (vector (list :type "text" :text payload))))
                   buffer)
                  (goto-char (point-max))
                  (redisplay t)))
          (setq rendered-size (buffer-size)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    (pi-coding-agent-large-output-bench--result-plist
     'history size nil elapsed rendered-size)))

;;;###autoload
(defun pi-coding-agent-large-output-bench-run-gui ()
  "Run visible GUI history/text/thinking workloads and return result plists."
  (unless (display-graphic-p)
    (user-error "Large-output GUI benchmark requires a graphical frame"))
  (let (results)
    (dolist (size pi-coding-agent-large-output-bench--sizes)
      (push (pi-coding-agent-large-output-bench--gui-history size) results)
      (push (pi-coding-agent-large-output-bench--gui-stream
             'text size 8192)
            results)
      (push (pi-coding-agent-large-output-bench--gui-stream
             'thinking size 8192)
            results))
    (nreverse results)))

(provide 'pi-coding-agent-large-output-bench)
;;; pi-coding-agent-large-output-bench.el ends here
