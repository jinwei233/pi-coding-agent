;;; pi-coding-agent-large-output-bench.el --- Large output benchmarks -*- lexical-binding: t; -*-

;;; Commentary:
;; Synthetic history and streaming workloads.  No user session files are read.

;;; Code:

(require 'benchmark)
(require 'pi-coding-agent)

(defconst pi-coding-agent-large-output-bench--sizes
  (list (* 500 1024) (* 1024 1024) (* 5 1024 1024)))

(defconst pi-coding-agent-large-output-bench--delta-sizes '(128 1024 8192))

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
                  (redisplay t)))
          (setq rendered-size (buffer-size)))
      (when (buffer-live-p buffer)
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
