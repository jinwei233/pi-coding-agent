;;; viewport-baseline.el --- Streaming viewport regression fixture -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared geometry assertions for fixed-height streaming viewport tests.

;;; Code:

(require 'ert)

(defconst pi-coding-agent-test-viewport-body-height 46
  "Reference Output window body height used by viewport regressions.")

(defconst pi-coding-agent-test-viewport-deltas '(1 5 20 50)
  "Single-line and batch sizes that exercise bounded tail movement.")

(defun pi-coding-agent-test-viewport-lines (prefix count &optional offset)
  "Return COUNT newline-terminated lines named PREFIX, starting at OFFSET."
  (let ((offset (or offset 0)))
    (mapconcat
     (lambda (index)
       (format "%s %03d\n" prefix (+ offset index)))
     (number-sequence 1 count)
     "")))

(defun pi-coding-agent-test-viewport-last-content-position ()
  "Return the display position after the final non-whitespace character."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\n\r")
    (point)))

(defun pi-coding-agent-test-viewport-tail-row (window)
  "Return the zero-based screen row of the last content line in WINDOW."
  (let ((tail (pi-coding-agent-test-viewport-last-content-position)))
    (when (> tail (point-min))
      (max 0
           (1- (count-screen-lines
                (window-start window) tail nil window))))))

(defun pi-coding-agent-test-viewport-geometry (window)
  "Return stable viewport evidence for WINDOW."
  (let* ((tail-row (pi-coding-agent-test-viewport-tail-row window))
         (body-height (window-body-height window)))
    (list :body-height body-height
          :start (window-start window)
          :start-line (line-number-at-pos (window-start window))
          :tail-row tail-row
          :blank-rows (and tail-row (- body-height tail-row 1))
          :generation
          (window-parameter
           window 'pi-coding-agent-streaming-scroll-generation)
          :vscroll (window-vscroll window)
          :point (window-point window)
          :point-max (point-max))))

(defun pi-coding-agent-test-viewport-append-lines
    (prefix count &optional offset)
  "Append COUNT fixture lines named PREFIX, starting at OFFSET."
  (pi-coding-agent--append-to-chat
   (pi-coding-agent-test-viewport-lines prefix count offset)))

(defun pi-coding-agent-test-viewport-set-body-height (window height)
  "Resize WINDOW's frame until its body has HEIGHT rows."
  (dotimes (_ 3)
    (let ((delta (- height (window-body-height window))))
      (unless (zerop delta)
        (set-frame-height
         (window-frame window)
         (+ (frame-height (window-frame window)) delta))
        (redisplay t))))
  (should (= (window-body-height window) height)))

(defun pi-coding-agent-test-viewport-assert-fixed-tail
    (window padding)
  "Assert that WINDOW has fixed PADDING below its final content row."
  (let ((geometry (pi-coding-agent-test-viewport-geometry window)))
    (should
     (= (plist-get geometry :tail-row)
        (- (plist-get geometry :body-height) padding 1)))
    (should (= (plist-get geometry :blank-rows) padding))
    (should (= (plist-get geometry :vscroll) 0))
    (should (= (plist-get geometry :point)
               (plist-get geometry :point-max)))
    (should-not (plist-get geometry :generation))))

(defun pi-coding-agent-test-viewport-assert-bounded-append
    (before-start-line after-start-line delta)
  "Assert a pure append moved start monotonically by at most DELTA lines."
  (let ((movement (- after-start-line before-start-line)))
    (should (>= movement 0))
    (should (<= movement delta))))

(provide 'viewport-baseline)
;;; viewport-baseline.el ends here
