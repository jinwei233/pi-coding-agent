;;; pi-coding-agent-gui-tests.el --- GUI integration tests for pi-coding-agent -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT tests that require a real Emacs GUI (windows, frames, scrolling).
;; Run with: make test-gui [SELECTOR=pattern]
;;
;; These tests focus on behavior that CANNOT be tested with unit tests:
;; - Real window scrolling during streamed updates in a displayed buffer
;; - Auto-scroll vs scroll-preservation with deterministic fake scenarios
;; - Tool-block overlays and extension UI behavior through the subprocess seam
;;
;; Many behaviors (history, spacing, and linked-buffer teardown) are covered
;; more directly by unit tests.
;;
;; For quick fake-backed debugging with a visible window:
;;   ./test/run-gui-tests.sh pi-coding-agent-gui-test-scroll-auto-when-at-end

;;; Code:

(require 'ert)
(require 'pi-coding-agent-gui-test-utils)
(require 'pi-coding-agent-test-common)
(load (expand-file-name
       "fixtures/viewport-baseline.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun pi-coding-agent-gui-test--table-display-strings (beg end)
  "Return ordered table overlay display strings between BEG and END."
  (when-let ((buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (mapcar (lambda (ov) (overlay-get ov 'display))
              (sort (seq-filter
                     (lambda (ov) (overlay-get ov 'pi-coding-agent-table-display))
                     (overlays-in beg end))
                    (lambda (left right)
                      (< (overlay-start left) (overlay-start right))))))))

(defconst pi-coding-agent-gui-test--native-unicode-baseline
  `((:text "M" :codepoints (#x4d) :char-widths (1)
     :string-width 1 :wrapper-width 1 :font "LXGW WenKai Mono" :pixels 9)
    (:text "✎" :codepoints (#x270e) :char-widths (1)
     :string-width 1 :wrapper-width 1 :font "Arial Unicode MS" :pixels 16)
    (:text "🛠" :codepoints (#x1f6e0) :char-widths (1)
     :string-width 1 :wrapper-width 1 :font nil :pixels 22)
    (:text ,(string #x1f6e0 #xfe0f) :codepoints (#x1f6e0 #xfe0f)
     :char-widths (1 0) :string-width 2 :wrapper-width 2
     :font-components ("Twemoji LXGW") :pixels 18)
    (:text "⚙" :codepoints (#x2699) :char-widths (1)
     :string-width 1 :wrapper-width 1 :font "Menlo" :pixels 11)
    (:text ,(string #x2699 #xfe0f) :codepoints (#x2699 #xfe0f)
     :char-widths (1 0) :string-width 2 :wrapper-width 2
     :font-components ("Menlo" "Twemoji LXGW") :pixels 18)
    (:text ,(string #x26a0 #xfe0f) :codepoints (#x26a0 #xfe0f)
     :char-widths (1 0) :string-width 2 :wrapper-width 2
     :font-components ("LXGW WenKai Mono" "Twemoji LXGW") :pixels 18)
    (:text "❯" :codepoints (#x276f) :char-widths (1)
     :string-width 1 :wrapper-width 1
     :font "Symbols Nerd Font Mono" :pixels 18)
    (:text "◽" :codepoints (#x25fd) :char-widths (2)
     :string-width 2 :wrapper-width 2 :font "Twemoji LXGW" :pixels 18
     :legacy-char-widths (1) :legacy-string-width 1 :legacy-wrapper-width 1)
    (:text "◾" :codepoints (#x25fe) :char-widths (2)
     :string-width 2 :wrapper-width 2 :font "Twemoji LXGW" :pixels 18
     :legacy-char-widths (1) :legacy-string-width 1 :legacy-wrapper-width 1)
    (:text "🔧" :codepoints (#x1f527) :char-widths (2)
     :string-width 2 :wrapper-width 2 :font "Twemoji LXGW" :pixels 18)
    (:text "💭" :codepoints (#x1f4ad) :char-widths (2)
     :string-width 2 :wrapper-width 2 :font "Twemoji LXGW" :pixels 18)
    (:text "✅" :codepoints (#x2705) :char-widths (2)
     :string-width 2 :wrapper-width 2 :font "Twemoji LXGW" :pixels 18))
  "Measured macOS NS Emacs 31.1 character acceptance baseline.")

(defconst pi-coding-agent-gui-test--legacy-table-row-pixels
  '(("ordinary" . 558)
    ("pencil-lower-right" . 565)
    ("bare-hammer-and-wrench" . 571)
    ("bare-gear" . 560)
    ("turn-marker" . 567)
    ("white-squares" . 576)
    ("presentation" . 567))
  "Pre-fix pixel evidence from 62-column rows in the reported table.")

(defconst pi-coding-agent-gui-test--reported-symbol-table
  (concat
   "| Fixture | Character | Result |\n"
   "|---------|-----------|--------|\n"
   "| ordinary | ASCII | baseline |\n"
   "| pencil-lower-right | ✎ | raw fallback |\n"
   "| bare-hammer-and-wrench | 🛠 | normalize |\n"
   "| bare-gear | ⚙ | normalize |\n"
   "| turn-marker | ❯ | raw fallback |\n"
   "| white-squares | ◽ ◾ | two cells each |\n"
   "| presentation | 🛠️ ⚙️ ⚠️ 🔧 💭 ✅ | two cells each |\n")
  "Three-column mixed-symbol regression fixture.")

(defconst pi-coding-agent-gui-test--reported-measurement-table
  (concat
   "| char-width | string-width | wrapper width | codepoint | font-at | pixels |\n"
   "|------------|--------------|---------------|-----------|---------|--------|\n"
   "| 1 | 1 | 1 | U+270E | Arial Unicode MS | 16px |\n"
   "| 1 | 1 | 1 | U+1F6E0 | nil | 22px |\n"
   "| 1/0 | 2 | 2 | U+1F6E0+FE0F | Twemoji LXGW | 18px |\n")
  "Six-column dense measurement fixture that must fall back when narrow.")

(defconst pi-coding-agent-gui-test--u270e-font-scan
  '(("Menlo" . 11)
    ("Lucida Grande" . 14)
    ("Arial Unicode MS" . 16))
  "Measured U+270E advances; none equals the 9px ASCII cell.")

(defun pi-coding-agent-gui-test--configured-native-font-stack-p ()
  "Return non-nil for the supported configured macOS NS test frame."
  (and (display-graphic-p)
       (eq window-system 'ns)
       (equal (face-attribute 'default :family nil t)
              "LXGW WenKai Mono")
       (= (string-pixel-width "M") 9)))

(defun pi-coding-agent-gui-test--measure-native-string (text window)
  "Measure TEXT in WINDOW and return stable width/font metadata."
  (let* ((font (font-at 1 window))
         (family (and font (font-get font :family))))
    (list :text text
          :codepoints (string-to-list text)
          :char-widths (mapcar #'char-width (string-to-list text))
          :string-width (string-width text)
          :wrapper-width (markdown-table-wrap-visible-width text)
          :font (and family
                     (replace-regexp-in-string
                      "\\\\ " " " (format "%s" family)))
          :pixels (string-pixel-width text))))

(defun pi-coding-agent-gui-test--table-overlays-in (buffer beg end)
  "Return ordered table display overlays in BUFFER between BEG and END."
  (with-current-buffer buffer
    (sort
     (seq-filter
      (lambda (overlay)
        (overlay-get overlay 'pi-coding-agent-table-display))
      (overlays-in beg end))
     (lambda (left right)
       (< (overlay-start left) (overlay-start right))))))

(defun pi-coding-agent-gui-test--thinking-scroll-lines (prefix count)
  "Return COUNT newline-separated lines starting with PREFIX."
  (mapconcat (lambda (n) (format "%s %02d" prefix n))
             (number-sequence 1 count)
             "\n"))

(defun pi-coding-agent-gui-test--thinking-scroll-messages ()
  "Return canonical history that exercises thinking-display scroll behavior."
  (vector
   (list :role "user"
         :content (vector (list :type "text" :text "Question one"))
         :timestamp 1704067200000)
   (list :role "assistant"
         :content (vector (list :type "text"
                                :text (concat
                                       (pi-coding-agent-gui-test--thinking-scroll-lines
                                        "Earlier assistant line" 18)
                                       "\n\nShort bridge.")))
         :timestamp 1704067200500)
   (list :role "user"
         :content (vector (list :type "text" :text "Question two"))
         :timestamp 1704067200800)
   (list :role "assistant"
         :content (vector
                   (list :type "text"
                         :text "Prelude line A\nPrelude line B\nPrelude line C")
                   (list :type "thinking"
                         :thinking (concat
                                    (pi-coding-agent-gui-test--thinking-scroll-lines
                                     "Thinking detail" 30)
                                    "\n\nConclusion thought"))
                   (list :type "text"
                         :text (concat "\n"
                                       (pi-coding-agent-gui-test--thinking-scroll-lines
                                        "Final answer line" 12))))
         :timestamp 1704067201000)))

(defun pi-coding-agent-gui-test--thinking-scroll-hidden-stub ()
  "Return the collapsed thinking stub used by the scroll-history fixture."
  (pi-coding-agent--thinking-hidden-stub
   (pi-coding-agent--thinking-normalize-text
    (concat
     (pi-coding-agent-gui-test--thinking-scroll-lines
      "Thinking detail" 30)
     "\n\nConclusion thought"))))

(defun pi-coding-agent-gui-test--render-thinking-scroll-history (buffer display-mode)
  "Render scroll-regression history into BUFFER using DISPLAY-MODE."
  (with-current-buffer buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--status 'idle
          pi-coding-agent--thinking-display display-mode
          pi-coding-agent--canonical-messages
          (pi-coding-agent-gui-test--thinking-scroll-messages))
    (pi-coding-agent--display-session-history pi-coding-agent--canonical-messages
                                              buffer)
    (font-lock-ensure)
    (redisplay)))

(defun pi-coding-agent-gui-test--window-visible-screen-lines (window)
  "Return how many screen lines WINDOW currently shows from buffer text."
  (count-screen-lines (window-start window) (window-end window t) nil window))

(defun pi-coding-agent-gui-test--window-point-row (window)
  "Return WINDOW point's screen-line row within the current viewport."
  (count-screen-lines (window-start window) (window-point window) nil window))

(defun pi-coding-agent-gui-test--window-current-line (window)
  "Return WINDOW point's current line without text properties."
  (with-selected-window window
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun pi-coding-agent-gui-test--window-thinking-block-order (window)
  "Return the completed-thinking block order at WINDOW point, or nil."
  (with-selected-window window
    (pi-coding-agent--thinking-block-at-pos (point))))

(defun pi-coding-agent-gui-test--window-substantially-filled-p (window)
  "Return non-nil when WINDOW shows nearly a full body of buffer text."
  (>= (pi-coding-agent-gui-test--window-visible-screen-lines window)
      (- (window-body-height window) 2)))

;;;; Session Tests

(ert-deftest pi-coding-agent-gui-test-session-starts ()
  "Test that a fake-backed pi session starts with proper layout."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "prompt-lifecycle")
    (should (pi-coding-agent-gui-test-session-active-p))
    (should (pi-coding-agent-gui-test-chat-window))
    (should (pi-coding-agent-gui-test-input-window))
    (should (pi-coding-agent-gui-test-verify-layout))))

;;;; Scroll Preservation Tests

(ert-deftest pi-coding-agent-gui-test-streaming-keeps-fixed-tail-padding ()
  "Batch appends keep a 46-row Output pinned above two blank rows."
  (let ((buffer (generate-new-buffer " *pi-stream-tail-gui*"))
        (input-buffer (generate-new-buffer " *pi-stream-tail-input*"))
        (frame (selected-frame))
        (original-height (frame-height)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let* ((window (selected-window))
                 (input-window (split-window window -4 'below)))
            (set-window-buffer window buffer)
            (set-window-buffer input-window input-buffer)
            (pi-coding-agent-test-viewport-set-body-height
             window pi-coding-agent-test-viewport-body-height)
            (with-current-buffer input-buffer
              (insert "Input draft\nsecond line\n"))
            (set-window-start input-window (point-min))
            (set-window-point input-window (point-min))
            (let ((input-start (window-start input-window))
                  (input-point (window-point input-window)))
              (with-current-buffer buffer
                (pi-coding-agent-chat-mode)
                (setq-local pi-coding-agent-follow-tail-padding-lines 2)
                (let ((inhibit-read-only t))
                  (insert
                   (pi-coding-agent-test-viewport-lines
                    "Earlier line"
                    (+ 10 pi-coding-agent-test-viewport-body-height))))
                (set-window-point window (point-max))
                (pi-coding-agent--settle-following-window window)
                (select-window input-window)
                (let ((offset 0))
                  (dolist (delta pi-coding-agent-test-viewport-deltas)
                    (let ((before
                           (with-current-buffer buffer
                             (line-number-at-pos (window-start window)))))
                      (with-current-buffer buffer
                        (pi-coding-agent-test-viewport-append-lines
                         "Reply line" delta offset))
                      (setq offset (+ offset delta))
                      (redisplay t)
                      (with-current-buffer buffer
                        (pi-coding-agent-test-viewport-assert-fixed-tail
                         window 2)
                        (pi-coding-agent-test-viewport-assert-bounded-append
                         before
                         (line-number-at-pos (window-start window))
                         delta)))))
                (should (eq (selected-window) input-window))
                (should (= (window-start input-window) input-start))
                (should (= (window-point input-window) input-point))
                (should
                 (equal
                  (with-current-buffer input-buffer (buffer-string))
                  "Input draft\nsecond line\n"))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p input-buffer)
        (kill-buffer input-buffer))
      (set-frame-height frame original-height))))

(ert-deftest pi-coding-agent-gui-test-tail-padding-content-matrix ()
  "Tail padding survives whitespace, wrapping, Unicode, code, and tables."
  (let ((buffer (generate-new-buffer " *pi-tail-content-matrix*"))
        (frame (selected-frame))
        (original-width (frame-width)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((window (selected-window))
                (cases
                 '(("plain tail" "")
                   ("plain tail" "\n")
                   ("plain tail" "\n\n")
                   ("plain tail" " \t\n\n")
                   ("中文结尾" "\n\n")
                   ("emoji 🙂 tail" "\n\n")
                   ("This final line is intentionally long enough to wrap across several visual rows in a narrow Output window while preserving the same tail boundary."
                    "\n")
                   ("```elisp\n(message \"tail\")\n```" "\n\n")
                   ("| key | value |\n|---|---|\n| tail | 中文🙂 |" "\n\n"))))
            (set-window-buffer window buffer)
            (with-current-buffer buffer
              (pi-coding-agent-chat-mode)
              (setq-local pi-coding-agent-follow-tail-padding-lines 2)
              (dolist (width '(70 96))
                (set-frame-width frame width)
                (redisplay t)
                (dolist (case cases)
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert
                     (pi-coding-agent-test-viewport-lines "Earlier line" 60))
                    (insert (car case) (cadr case)))
                  (when (string-prefix-p "|" (car case))
                    (pi-coding-agent--decorate-tables-in-region
                     (line-beginning-position -5) (point-max)
                     (window-body-width window)))
                  (set-window-point window (point-max))
                  (pi-coding-agent--settle-following-window window)
                  (redisplay t)
                  (ert-info ((format "width=%d content=%S suffix=%S"
                                     width (car case) (cadr case)))
                    (if (string-match-p "[^[:ascii:]]" (car case))
                        (let ((geometry
                               (pi-coding-agent-test-viewport-geometry window)))
                          (should (<= 2 (plist-get geometry :blank-rows) 3))
                          (should (zerop (plist-get geometry :vscroll)))
                          (should (= (plist-get geometry :point)
                                     (plist-get geometry :point-max))))
                      (pi-coding-agent-test-viewport-assert-fixed-tail
                       window 2))
                    (let ((start (window-start window)))
                      (pi-coding-agent--settle-following-window window)
                      (redisplay t)
                      (should (= (window-start window) start))
                      (if (string-match-p "[^[:ascii:]]" (car case))
                          (let ((geometry
                                 (pi-coding-agent-test-viewport-geometry window)))
                            (should
                             (<= 2 (plist-get geometry :blank-rows) 3)))
                        (pi-coding-agent-test-viewport-assert-fixed-tail
                         window 2)))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (set-frame-width frame original-width))))

(ert-deftest pi-coding-agent-gui-test-resize-settles-projection-contraction ()
  "A wider, shorter Output immediately settles its contracted projection."
  (let ((buffer (generate-new-buffer " *pi-tail-resize-gui*"))
        (input-buffer (generate-new-buffer " *pi-tail-resize-input*"))
        (frame (selected-frame))
        (original-width (frame-width))
        (original-height (frame-height)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-frame-width frame 70)
          (let* ((window (selected-window))
                 (input-window (split-window window -4 'below)))
            (set-window-buffer window buffer)
            (set-window-buffer input-window input-buffer)
            (pi-coding-agent-test-viewport-set-body-height
             window pi-coding-agent-test-viewport-body-height)
            (with-current-buffer buffer
              (pi-coding-agent-chat-mode)
              (setq-local pi-coding-agent-follow-tail-padding-lines 2)
              (let ((inhibit-read-only t))
                (dotimes (index 80)
                  (insert
                   (format
                    "line %03d wraps at seventy columns but contracts after the frame becomes wider\n"
                    index))))
              (set-window-point window (point-max))
              (pi-coding-agent--settle-following-window window)
              (redisplay t)
              (pi-coding-agent-test-viewport-assert-fixed-tail window 2)
              (let ((before-screen-lines
                     (count-screen-lines
                      (point-min)
                      (pi-coding-agent-test-viewport-last-content-position)
                      nil window))
                    (before-body-height (window-body-height window))
                    (before-settles pi-coding-agent--tail-settle-count))
                (select-window input-window)
                (set-frame-size
                 frame 96 (- (frame-height frame) 8))
                (redisplay t)
                (should
                 (pi-coding-agent-test-wait-until
                  (lambda ()
                    (with-current-buffer buffer
                      (let ((geometry
                             (pi-coding-agent-test-viewport-geometry window)))
                        (and (/= (window-body-height window)
                                 before-body-height)
                             (= (plist-get geometry :blank-rows) 2)
                             (zerop (plist-get geometry :vscroll))))))
                  2 0.05))
                (with-current-buffer buffer
                  (should
                   (< (count-screen-lines
                       (point-min)
                       (pi-coding-agent-test-viewport-last-content-position)
                       nil window)
                      before-screen-lines))
                  (should (> pi-coding-agent--tail-settle-count
                             before-settles))
                  (pi-coding-agent-test-viewport-assert-fixed-tail window 2))
                (should (eq (selected-window) input-window))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p input-buffer)
        (kill-buffer input-buffer))
      (set-frame-size frame original-width original-height)
      (redisplay t))))

(ert-deftest pi-coding-agent-gui-test-natural-tail-preserves-pixel-vscroll ()
  "Nil and zero padding do not clear native pixel vscroll."
  (let ((buffer (generate-new-buffer " *pi-natural-tail-vscroll*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((window (selected-window)))
            (set-window-buffer window buffer)
            (with-current-buffer buffer
              (pi-coding-agent-chat-mode)
              (let ((inhibit-read-only t))
                (insert
                 (pi-coding-agent-test-viewport-lines "Natural line" 60)))
              (dolist (padding '(nil 0))
                (setq-local pi-coding-agent-follow-tail-padding-lines padding)
                (set-window-point window (point-max))
                (set-window-vscroll window 1)
                (let ((vscroll (window-vscroll window)))
                  (pi-coding-agent--settle-following-window window)
                  (should (= (window-vscroll window) vscroll)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest pi-coding-agent-gui-test-scroll-preserved-streaming ()
  "Test scroll position is preserved while a fake stream updates below."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "scrolling-text")
    (pi-coding-agent-gui-test-send "first turn")
    ;; The fake stream is usually tall enough already, but large frames can make
    ;; the buffer barely non-scrollable.  Top off with dummy lines so the test
    ;; exercises scroll preservation rather than frame geometry.
    (pi-coding-agent-gui-test-ensure-scrollable)
    (pi-coding-agent-gui-test-scroll-up 20)
    (should-not (pi-coding-agent-gui-test-at-end-p))
    (let ((line-before (pi-coding-agent-gui-test-top-line-number)))
      (should (> line-before 1))
      (pi-coding-agent-gui-test-send "second turn")
      (should (= line-before (pi-coding-agent-gui-test-top-line-number)))
      (should (pi-coding-agent-gui-test-chat-contains "Scroll line 24 for second turn")))))

(ert-deftest pi-coding-agent-gui-test-scroll-preserved-tool-use ()
  "Test scroll position is preserved while fake tool output arrives."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "tool-read")
    (pi-coding-agent-gui-test-ensure-scrollable)
    (pi-coding-agent-gui-test-scroll-up 20)
    (should-not (pi-coding-agent-gui-test-at-end-p))
    (let ((line-before (pi-coding-agent-gui-test-top-line-number)))
      (should (> line-before 1))
      (pi-coding-agent-gui-test-send "Use the fake read tool")
      (should (= line-before (pi-coding-agent-gui-test-top-line-number)))
      (should (pi-coding-agent-gui-test-chat-contains
               "read /tmp/fake-tool.txt"))
      (should-not (pi-coding-agent-gui-test-chat-contains
                   "fake tool output")))))

(ert-deftest pi-coding-agent-gui-test-scroll-auto-when-at-end ()
  "Test auto-scroll when user is at end across deterministic fake turns.
Regression: `pi-coding-agent--display-agent-end' must leave window-point at
buffer end so the next streamed turn still follows automatically."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "scrolling-text")
    (pi-coding-agent-gui-test-send "first turn")
    (should (pi-coding-agent-gui-test-window-point-at-end-p))
    (should (pi-coding-agent-gui-test-at-end-p))
    (pi-coding-agent-gui-test-send "second turn")
    (should (pi-coding-agent-gui-test-window-point-at-end-p))
    (should (pi-coding-agent-gui-test-at-end-p))
    (should (pi-coding-agent-gui-test-chat-contains "Scroll line 24 for second turn"))))

(ert-deftest pi-coding-agent-gui-test-thinking-display-toggle-keeps-tail-filled ()
  "Toggling completed thinking keeps a tail-following chat window filled.
After the toggle, a new streamed turn should still auto-scroll from the tail."
  (let ((buf (get-buffer-create "*pi-gui-thinking-tail*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pi-coding-agent-gui-test--render-thinking-scroll-history buf 'visible)
          (let ((win (selected-window)))
            (goto-char (point-max))
            (recenter -1)
            (redisplay)
            (should (>= (window-point win) (1- (point-max))))
            (should (pi-coding-agent-gui-test--window-substantially-filled-p win))
            (pi-coding-agent-toggle-thinking-display)
            (redisplay)
            (should (string-match-p
                     (regexp-quote
                      (pi-coding-agent-gui-test--thinking-scroll-hidden-stub))
                     (buffer-string)))
            (should (>= (window-point win) (1- (point-max))))
            (should (>= (window-end win t) (1- (point-max))))
            (should (pi-coding-agent-gui-test--window-substantially-filled-p win))
            (pi-coding-agent--display-agent-start)
            (pi-coding-agent--display-message-delta "Streaming tail line 01\n")
            (redisplay)
            (should (>= (window-point win) (1- (point-max))))
            (should (>= (window-end win t) (1- (point-max))))
            (pi-coding-agent--display-message-delta "Streaming tail line 02\n")
            (pi-coding-agent--display-agent-end)
            (redisplay)
            (should (>= (window-point win) (1- (point-max))))
            (should (>= (window-end win t) (1- (point-max))))
            (should (string-match-p "Streaming tail line 02" (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-gui-test-thinking-display-toggle-keeps-context-window-usable ()
  "Toggling completed thinking keeps a non-tail chat window usable.
Point should stay on the same logical thinking block and the window should
remain substantially filled even when the collapsed tail is shorter."
  (let ((buf (get-buffer-create "*pi-gui-thinking-context*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pi-coding-agent-gui-test--render-thinking-scroll-history buf 'visible)
          (let ((win (selected-window))
                block-before)
            (goto-char (point-min))
            (should (search-forward "Thinking detail 25" nil t))
            (beginning-of-line)
            (recenter 10)
            (redisplay)
            (setq block-before
                  (pi-coding-agent-gui-test--window-thinking-block-order win))
            (should block-before)
            (should (pi-coding-agent-gui-test--window-substantially-filled-p win))
            (should (equal "> Thinking detail 25"
                           (pi-coding-agent-gui-test--window-current-line win)))
            (pi-coding-agent-toggle-thinking-display)
            (redisplay)
            (should (string-match-p
                     (regexp-quote
                      (pi-coding-agent-gui-test--thinking-scroll-hidden-stub))
                     (buffer-string)))
            (should (equal (pi-coding-agent-gui-test--thinking-scroll-hidden-stub)
                           (pi-coding-agent-gui-test--window-current-line win)))
            (should (equal block-before
                           (pi-coding-agent-gui-test--window-thinking-block-order win)))
            (should (pi-coding-agent-gui-test--window-substantially-filled-p win))))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-gui-test-thinking-display-toggle-expands-same-block-from-hidden-stub ()
  "Expanding a hidden thinking stub restores the same logical block.
Point should land back on the same completed-thinking block rather than an
unrelated raw offset in the conversation."
  (let ((buf (get-buffer-create "*pi-gui-thinking-expand*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pi-coding-agent-gui-test--render-thinking-scroll-history buf 'hidden)
          (let ((win (selected-window))
                block-before)
            (goto-char (point-min))
            (should (search-forward
                     (pi-coding-agent-gui-test--thinking-scroll-hidden-stub)
                     nil t))
            (beginning-of-line)
            (recenter 10)
            (redisplay)
            (setq block-before
                  (pi-coding-agent-gui-test--window-thinking-block-order win))
            (should block-before)
            (should (equal (pi-coding-agent-gui-test--thinking-scroll-hidden-stub)
                           (pi-coding-agent-gui-test--window-current-line win)))
            (pi-coding-agent-toggle-thinking-display)
            (redisplay)
            (should (string-match-p "Thinking detail 25" (buffer-string)))
            (should (equal block-before
                           (pi-coding-agent-gui-test--window-thinking-block-order win)))
            (should (equal "▾ Think"
                           (pi-coding-agent-gui-test--window-current-line win)))
            (should (pi-coding-agent-gui-test--window-substantially-filled-p win))))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-gui-test-thinking-display-toggle-restores-each-visible-window ()
  "Toggling completed thinking preserves each visible window's own context.
The tail window should stay at the end, while the context window keeps its
logical block and should not start following later streamed output."
  (let ((buf (get-buffer-create "*pi-gui-thinking-multi-window*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buf)
          (pi-coding-agent-gui-test--render-thinking-scroll-history buf 'visible)
          (let* ((tail-win (selected-window))
                 (context-win (split-window-below)))
            (set-window-buffer context-win buf)
            (with-selected-window tail-win
              (goto-char (point-max))
              (recenter -1)
              (redisplay))
            (with-selected-window context-win
              (goto-char (point-min))
              (should (search-forward "Thinking detail 25" nil t))
              (beginning-of-line)
              (recenter 8)
              (redisplay))
            (let ((context-line-before
                   (pi-coding-agent-gui-test--window-current-line context-win))
                  context-start-after-toggle)
              (with-selected-window tail-win
                (pi-coding-agent-toggle-thinking-display))
              (redisplay)
              (should (eq (selected-window) tail-win))
              (should (>= (window-point tail-win) (1- (with-current-buffer buf (point-max)))))
              (should (>= (window-end tail-win t)
                          (1- (with-current-buffer buf (point-max)))))
              (should (pi-coding-agent-gui-test--window-substantially-filled-p tail-win))
              (should (equal "> Thinking detail 25" context-line-before))
              (should (equal (pi-coding-agent-gui-test--thinking-scroll-hidden-stub)
                             (pi-coding-agent-gui-test--window-current-line context-win)))
              (should (pi-coding-agent-gui-test--window-substantially-filled-p context-win))
              (setq context-start-after-toggle (window-start context-win))
              (with-current-buffer buf
                (pi-coding-agent--display-agent-start)
                (pi-coding-agent--display-message-delta "Dual window tail line\n"))
              (redisplay)
              (should (>= (window-point tail-win) (1- (with-current-buffer buf (point-max)))))
              (should (>= (window-end tail-win t)
                          (1- (with-current-buffer buf (point-max)))))
              (should (= context-start-after-toggle (window-start context-win)))
              (should (equal (pi-coding-agent-gui-test--thinking-scroll-hidden-stub)
                             (pi-coding-agent-gui-test--window-current-line context-win))))))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-gui-test-table-resize-refreshes-hot-tail-only ()
  "Resizing the frame rewraps hot tables only and preserves scroll position."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "prompt-lifecycle")
    (let* ((chat-buf (plist-get pi-coding-agent-gui-test--session :chat-buffer))
           (frame (selected-frame))
           (orig-width (frame-width))
           (cold-table
            "| Feature | Notes |\n|---------|-------|\n| Cold history | This older table was wrapped at the original wide width and should stay frozen after resize |\n")
           (hot-table
            "| Feature | Notes |\n|---------|-------|\n| Hot tail | This recent table should rewrap when the window narrows so the columns remain readable |\n")
           cold-before
           hot-before
           cold-start
           hot-start)
      (unwind-protect
          (progn
            (with-current-buffer chat-buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "You · 10:00\n===========\n")
                (setq cold-start (point))
                (insert cold-table "\nAssistant\n=========\nRecent reply\n\nYou · 10:05\n===========\n")
                (setq hot-start (point))
                (insert hot-table)
                (dotimes (i 80)
                  (insert (format "filler line %d\n" i))))
              (font-lock-ensure)
              (let* ((chat-win (pi-coding-agent-gui-test-chat-window))
                     (initial-width (window-width chat-win)))
                (pi-coding-agent--decorate-tables-in-region
                 (point-min) (point-max) initial-width)
                (move-marker pi-coding-agent--hot-tail-start hot-start)
                (setq cold-before
                      (pi-coding-agent-gui-test--table-display-strings
                       cold-start hot-start)
                      hot-before
                      (pi-coding-agent-gui-test--table-display-strings
                       hot-start (point-max)))))
            (redisplay)
            (pi-coding-agent-gui-test-scroll-up 20)
            (let ((line-before (pi-coding-agent-gui-test-top-line-number)))
              (set-frame-size frame (- orig-width 30) (frame-height))
              (redisplay)
              (should (pi-coding-agent-test-wait-until
                       (lambda ()
                         (not (equal hot-before
                                     (pi-coding-agent-gui-test--table-display-strings
                                      hot-start (point-max)))))
                       2 0.05))
              (should (equal cold-before
                             (pi-coding-agent-gui-test--table-display-strings
                              cold-start hot-start)))
              (should (= line-before (pi-coding-agent-gui-test-top-line-number)))))
        (set-frame-size frame orig-width (frame-height))
        (redisplay)))))

(ert-deftest pi-coding-agent-gui-test-native-unicode-character-baseline ()
  "Configured NS frame matches the recorded character-width evidence."
  (skip-unless
   (pi-coding-agent-gui-test--configured-native-font-stack-p))
  (let ((buffer (generate-new-buffer " *pi-native-unicode-baseline*"))
        (window (selected-window)))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (dolist (fixture pi-coding-agent-gui-test--native-unicode-baseline)
              (let ((text (plist-get fixture :text)))
                (erase-buffer)
                (insert text)
                (redisplay t)
                (let ((actual
                       (pi-coding-agent-gui-test--measure-native-string
                        text window)))
                  (dolist (field
                           '(:codepoints :char-widths :string-width
                             :wrapper-width :pixels))
                    (should
                     (equal (plist-get actual field)
                            (plist-get fixture field))))
                  (when-let* ((expected-font (plist-get fixture :font)))
                    (should
                     (equal (plist-get actual :font) expected-font))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest pi-coding-agent-gui-test-native-u270e-font-scan-baseline ()
  "Known U+270E fonts remain non-cellular, preserving raw fallback."
  (skip-unless
   (pi-coding-agent-gui-test--configured-native-font-stack-p))
  (let ((buffer (generate-new-buffer " *pi-u270e-font-scan*"))
        (window (selected-window))
        advances)
    (unwind-protect
        (save-window-excursion
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (dolist (fixture pi-coding-agent-gui-test--u270e-font-scan)
              (erase-buffer)
              (insert
               (propertize "✎" 'face `(:family ,(car fixture))))
              (redisplay t)
              (let ((pixels (string-pixel-width
                             (buffer-substring (point-min) (point-max)))))
                (push pixels advances)
                (should (= pixels (cdr fixture)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))
    (should-not (memq 9 advances))
    (should
     (equal "✎"
            (pi-coding-agent--normalize-table-display-cell "✎")))))

(ert-deftest pi-coding-agent-gui-test-native-table-pixel-gate-and-fallback ()
  "Unsafe tables stay raw while safe normalized grids align at every border."
  (skip-unless
   (pi-coding-agent-gui-test--configured-native-font-stack-p))
  (let ((buffer (generate-new-buffer " *pi-native-table-safety*"))
        (window (selected-window))
        unsafe-beg unsafe-end safe-beg safe-end)
    (unwind-protect
        (save-window-excursion
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (pi-coding-agent-chat-mode)
            (let ((inhibit-read-only t))
              (setq unsafe-beg (point))
              (insert
               "| Symbol | Result |\n"
               "|--------|--------|\n"
               "| ✎ | raw |\n"
               "| ❯ | raw |\n")
              (setq unsafe-end (point))
              (insert "\n")
              (setq safe-beg (point))
              (insert
               "| Symbol | Result |\n"
               "|--------|--------|\n"
               "| 🛠 | normalized |\n"
               "| ⚙ | normalized |\n"
               "| ⚠️ | selected |\n"
               "| `🔧` | inline code |\n")
              (setq safe-end (point)))
            (font-lock-ensure)
            (pi-coding-agent--decorate-tables-in-region
             (point-min) (point-max) 80)
            (redisplay t)
            (should-not
             (pi-coding-agent-gui-test--table-overlays-in
              buffer unsafe-beg unsafe-end))
            (let* ((overlays
                    (pi-coding-agent-gui-test--table-overlays-in
                     buffer safe-beg safe-end))
                   (display-lines
                    (apply
                     #'append
                     (mapcar
                      (lambda (overlay)
                        (split-string
                         (string-trim-right
                          (overlay-get overlay 'display) "\n+")
                         "\n"))
                      overlays)))
                   (row-lines
                    (seq-filter
                     (lambda (line) (string-search "│" line))
                     display-lines))
                   (border-positions
                    (mapcar
                     (lambda (line)
                       (let (positions)
                         (dotimes (index (length line))
                           (when (= (aref line index) ?│)
                             (push
                              (string-pixel-width
                               (substring line 0 (1+ index)))
                              positions)))
                         (nreverse positions)))
                     row-lines))
                   (pixel-widths
                    (mapcar #'string-pixel-width display-lines)))
              (should overlays)
              (should
               (seq-some
                (lambda (line)
                  (string-search (string #x1f6e0 #xfe0f) line))
                display-lines))
              (should
               (seq-some
                (lambda (line)
                  (string-search (string #x2699 #xfe0f) line))
                display-lines))
              (should (= 1 (length (delete-dups pixel-widths))))
              (should (= 1 (length (delete-dups border-positions))))
              (should
               (cl-every
                (lambda (line)
                  (pi-coding-agent--table-line-pixel-safe-p
                   line window 9))
                display-lines)))
            (should
             (string-search "| ✎ | raw |" (buffer-string)))
            (should
             (string-search "| 🛠 | normalized |" (buffer-string)))
            (should-not
             (string-search
              (string #x1f6e0 #xfe0f)
              (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest pi-coding-agent-gui-test-reported-table-fixtures-are-complete ()
  "Regression fixtures retain every reported row and legacy pixel datum."
  (should
   (equal (mapcar #'cdr pi-coding-agent-gui-test--legacy-table-row-pixels)
          '(558 565 571 560 567 576 567)))
  (dolist (needle '("✎" "🛠" "⚙" "⚙️" "⚠️" "❯"
                    "◽" "◾" "🔧" "💭" "✅"))
    (should
     (string-search needle
                    pi-coding-agent-gui-test--reported-symbol-table)))
  (dolist (needle '("char-width" "string-width" "wrapper width"
                    "U+270E" "U+1F6E0+FE0F" "font-at" "16px"))
    (should
     (string-search needle
                    pi-coding-agent-gui-test--reported-measurement-table))))

;;;; Content Tests

(ert-deftest pi-coding-agent-gui-test-content-tool-output-shown ()
  "Test that tool output is summarized, then recoverable through TAB."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "tool-read")
    (pi-coding-agent-gui-test-send "Use the fake read tool")
    (should (pi-coding-agent-gui-test-chat-contains "read /tmp/fake-tool.txt"))
    (should (pi-coding-agent-gui-test-chat-text-in-tool-block-p
             "2 lines"))
    (should-not (pi-coding-agent-gui-test-chat-contains "fake tool output"))
    (should-not (pi-coding-agent-gui-test-chat-contains "TAB details"))
    (with-current-buffer (plist-get pi-coding-agent-gui-test--session
                                    :chat-buffer)
      (goto-char (point-min))
      (search-forward "2 lines")
      ;; The whole summary line is the toggle button now.
      (pi-coding-agent--toggle-tool-output
       (button-at (1- (point)))))
    (should (pi-coding-agent-gui-test-chat-text-in-tool-block-p
             "fake tool output"))
    (should (pi-coding-agent-gui-test-chat-contains "Tool finished"))))

(ert-deftest pi-coding-agent-gui-test-tool-overlay-bounded ()
  "Test that the tool overlay stops before later assistant text.
Regression: `pi-coding-agent--tool-overlay-finalize' must replace the
rear-advance overlay before assistant text continues after the tool block."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "tool-read")
    (pi-coding-agent-gui-test-send "Use the fake read tool")
    (with-current-buffer (plist-get pi-coding-agent-gui-test--session :chat-buffer)
      (goto-char (point-min))
      (search-forward "2 lines")
      (let* ((tool-pos (match-beginning 0))
             (tool-overlay (seq-find
                            (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                            (overlays-at tool-pos))))
        (should tool-overlay))
      (goto-char (point-min))
      (search-forward "Tool finished")
      (let* ((assistant-pos (match-beginning 0))
             (tool-overlay (seq-find
                            (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                            (overlays-at assistant-pos))))
        (should-not tool-overlay)))))

;;;; Extension Command Tests

(ert-deftest pi-coding-agent-gui-test-extension-command-returns-to-idle ()
  "Fake extension command without a visible turn returns to idle immediately."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "extension-noop")
    (pi-coding-agent-gui-test-send "/test-noop" t)
    (should (pi-coding-agent-gui-test-wait-for-idle 2))))

(ert-deftest pi-coding-agent-gui-test-extension-custom-message-displayed ()
  "Fake extension command displays a custom message in chat."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "extension-message")
    (pi-coding-agent-gui-test-send "/test-message")
    (should (pi-coding-agent-gui-test-chat-contains "Test message from extension"))))

(ert-deftest pi-coding-agent-gui-test-extension-confirm-response-displayed ()
  "Fake extension confirm response triggers the displayed follow-up message."
  (pi-coding-agent-gui-test-with-fresh-session
    (:backend fake :fake-scenario "extension-confirm")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (pi-coding-agent-gui-test-send "/test-confirm")
      (should (pi-coding-agent-gui-test-chat-contains "CONFIRMED")))))

;;;; Tool Toggle Tests

(ert-deftest pi-coding-agent-gui-test-tool-toggle-expand-collapse-cycle ()
  "TAB expands, collapses, and re-expands in a displayed GUI buffer.
Regression for issue #166: collapsing from the [-] button placed cursor
at the overlay boundary (half-open interval), making the next TAB fall
through to `outline-cycle' instead of toggling.  Uses a standalone
chat-mode buffer to isolate the toggle logic from RPC timing."
  (let ((buf (get-buffer-create "*pi-gui-toggle-test*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pi-coding-agent-chat-mode)
          (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.txt"))
          (pi-coding-agent--display-tool-end "read" '(:path "/tmp/test.txt")
            `((:type "text"
               :text ,(mapconcat (lambda (n) (format "Line %02d of file" n))
                                 (number-sequence 1 20) "\n")))
            nil nil)
          (font-lock-ensure)
          (redisplay)
          ;; Initially collapsed
          (should (string-match-p "more lines)" (buffer-string)))
          (should-not (string-match-p "Line 20" (buffer-string)))
          ;; Expand from the collapsed indicator
          (goto-char (point-min))
          (search-forward "more lines)" nil t)
          (beginning-of-line)
          (pi-coding-agent-toggle-tool-section)
          (redisplay)
          (should (string-match-p "Line 20" (buffer-string)))
          ;; Navigate to [-] button and collapse
          (goto-char (point-min))
          (search-forward "[-]" nil t)
          (beginning-of-line)
          (pi-coding-agent-toggle-tool-section)
          (redisplay)
          (should (string-match-p "more lines)" (buffer-string)))
          (should-not (string-match-p "Line 20" (buffer-string)))
          ;; Re-expand must still work from current cursor position
          (pi-coding-agent-toggle-tool-section)
          (redisplay)
          (should (string-match-p "Line 20" (buffer-string))))
      (kill-buffer buf))))

;;;; Grouped Activity Tests

(ert-deftest pi-coding-agent-gui-test-grouped-activity-narrow-themes ()
  "Grouped activity stays one line without button styling across themes."
  (let ((buffer (get-buffer-create "*pi-gui-grouped-activity*"))
        (original-themes custom-enabled-themes)
        (pi-coding-agent-activity-icon-style 'emoji))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (pi-coding-agent-chat-mode)
          (setq-local pi-coding-agent--tool-activity-display 'grouped)
          (pi-coding-agent--set-chat-session-identity "/tmp/pi-gui-activity/")
          (pi-coding-agent--display-session-history
           (vector
            '(:role "user" :content "check")
            '(:role "assistant"
              :content
              [(:type "toolCall" :id "call-1" :name "edit"
                :arguments
                (:path "/tmp/pi-gui-activity/very/long/path/file.el"))])
            '(:role "toolResult" :toolCallId "call-1" :toolName "edit"
              :content [(:type "text" :text "done")]
              :details (:diff "+ new\n- old") :isError nil))
           buffer)
          (dolist (theme '(modus-operandi modus-vivendi))
            (mapc #'disable-theme custom-enabled-themes)
            (load-theme theme t)
            (pi-coding-agent--update-theme-derived-faces)
            (font-lock-ensure)
            (redisplay t)
            (let ((group-pos
                   (text-property-any
                    (point-min) (point-max)
                    'pi-coding-agent-activity-group t)))
              (should group-pos)
              (goto-char group-pos)
              (let* ((line-start (line-beginning-position))
                     (line-end (line-end-position))
                  (line
                   (buffer-substring-no-properties
                     line-start line-end))
                     (separator
                      (save-excursion
                        (search-forward " · " line-end t)
                        (match-beginning 0))))
                (should separator)
                (should (= 4
                           (string-width
                            (buffer-substring-no-properties
                             line-start separator))))
                (should (<= (string-width line)
                            pi-coding-agent--single-line-preview-columns))
                (should-not (memq 'button (face-at-point nil t)))
                (should
                 (memq
                  (face-attribute
                   'pi-coding-agent-activity-label :underline nil t)
                  '(nil unspecified)))
                (should
                 (memq
                  (face-attribute
                   'pi-coding-agent-activity-summary :underline nil t)
                  '(nil unspecified)))
                (should
                 (eq 'pi-coding-agent-activity-summary
                     (get-text-property
                      (+ separator 3) 'font-lock-face)))))))
      (mapc #'disable-theme custom-enabled-themes)
      (mapc (lambda (theme) (load-theme theme t)) original-themes)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;;; Streaming Fontification Tests

(ert-deftest pi-coding-agent-gui-test-streaming-no-fences ()
  "Streaming write content shows no fence markers to the user.
Fences exist in the buffer for tree-sitter parsing, but
`md-ts-hide-markup' makes them invisible.  Uses a displayed
buffer (jit-lock active) to verify under real GUI conditions."
  (let ((buf (get-buffer-create "*pi-gui-fontify-test*")))
    (unwind-protect
        (progn
          (switch-to-buffer buf)
          (pi-coding-agent-chat-mode)
          (pi-coding-agent--handle-display-event '(:type "agent_start"))
          (pi-coding-agent--handle-display-event '(:type "message_start"))
          (pi-coding-agent--handle-display-event
           `(:type "message_update"
             :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
             :message (:role "assistant"
                       :content [(:type "toolCall" :id "call_1"
                                  :name "write"
                                  :arguments (:path "/tmp/test.py"))])))
          (redisplay)
          (pi-coding-agent-test--send-delta
           "write" '(:path "/tmp/test.py"
                     :content "def hello():\n    return 42\n"))
          (font-lock-ensure)
          ;; Fences are in the buffer (for tree-sitter) but invisible
          (let ((visible (pi-coding-agent--visible-text
                          (point-min) (point-max))))
            (should-not (string-match-p "```" visible)))
          ;; Content is present with syntax faces
          (goto-char (point-min))
          (should (search-forward "def" nil t))
          (let ((face (get-text-property (match-beginning 0) 'face)))
            (should (or (eq face 'font-lock-keyword-face)
                        (and (listp face)
                             (memq 'font-lock-keyword-face face))))))
      (kill-buffer buf))))

(provide 'pi-coding-agent-gui-tests)
;;; pi-coding-agent-gui-tests.el ends here
