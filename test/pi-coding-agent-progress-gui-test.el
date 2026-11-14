;;; pi-coding-agent-progress-gui-test.el --- Isolated progress GUI contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Deterministic GUI ERT contracts for segmented Output progress.  No Pi CLI,
;; model, session files, user init, or existing Emacs server is used.  Run in
;; a fresh graphical Emacs with the checkout and test directories on load-path.
;; The suite entry point/Makefile integration is deliberately owned elsewhere.
;; PI_PROGRESS_GUI_EVIDENCE optionally names an existing evidence directory.
;; Frame export, when supported, exports ONLY the test process's own frame.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pi-coding-agent-gui-test-utils)
(require 'pi-coding-agent-test-common)
(require 'pi-coding-agent-progress-fixture)

(defvar mode-line-window)

(defvar pi-coding-agent-progress-gui-test--evidence-directory
  (getenv "PI_PROGRESS_GUI_EVIDENCE")
  "Optional directory for isolated GUI assertion and frame-export evidence.")

(defun pi-coding-agent-progress-gui-test--lines (prefix count)
  "Return COUNT ordinary transcript lines prefixed by PREFIX."
  (mapconcat (lambda (number) (format "%s %03d" prefix number))
             (number-sequence 1 count) "\n"))

(defun pi-coding-agent-progress-gui-test--history (directory)
  "Return canonical history with two exact tool segments in DIRECTORY.
A long older answer supplies a real scrollable historical reading anchor.
The first segment contains a file read and command; the second only an edit.
Visible paths and command output deliberately contain misleading file names."
  (let ((path (expand-file-name "authoritative-source-with-a-long-name.el" directory)))
    (vector
     '(:role "user" :content "Earlier question")
     (list :role "assistant"
           :content (vector (list :type "text" :text
                                  (pi-coding-agent-progress-gui-test--lines
                                   "Historical reading anchor" 90))))
     '(:role "user" :content "Inspect, then change the source")
     (list :role "assistant"
           :content
           (vector
            '(:type "text" :text "Prelude A: inspect the actual source.")
            (pi-coding-agent-test--toolcall "gui-read" "read"
                                          (list :path path :offset 7))
            (pi-coding-agent-test--toolcall "gui-bash" "bash"
                                          '(:command "printf synthetic-check"))
            '(:type "text" :text "Bridge B: the model's actual explanation.")
            (pi-coding-agent-test--toolcall "gui-edit" "edit"
                                          (list :path path :oldText "old" :newText "new"))
            '(:type "text" :text "Final C: ordinary final answer remains readable.")))
     '(:role "toolResult" :toolCallId "gui-read" :toolName "read"
       :content [(:type "text" :text "MISLEADING /tmp/not-the-source.el:99\nread secret")])
     '(:role "toolResult" :toolCallId "gui-bash" :toolName "bash"
       :content [(:type "text" :text "GUI_COMMAND_DETAIL_ALPHA\nGUI_COMMAND_DETAIL_BETA")])
     '(:role "toolResult" :toolCallId "gui-edit" :toolName "edit"
       :content [(:type "text" :text "Synthetic edit completed")]
       :details (:diff "+ new\n- old")))))

(defun pi-coding-agent-progress-gui-test--visible ()
  "Return the visible transcript without properties."
  (substring-no-properties
   (pi-coding-agent--visible-text (point-min) (point-max))))

(defun pi-coding-agent-progress-gui-test--goto (text)
  "Move to TEXT in the current buffer."
  (goto-char (point-min))
  (unless (search-forward text nil t)
    (ert-fail (format "Missing fixture text: %s" text)))
  (goto-char (match-beginning 0)))

(defun pi-coding-agent-progress-gui-test--goto-segment (segment)
  "Move inside SEGMENT's disclosure row, not merely its first character."
  (goto-char (pi-coding-agent--activity-record-start-marker segment))
  (forward-char (min 3 (- (line-end-position) (point))))
  (should (eq segment (pi-coding-agent--activity-record-at-point)))
  (should (eq 'group (get-text-property (point) 'pi-coding-agent-activity-kind))))

(defun pi-coding-agent-progress-gui-test--key (key)
  "Execute KEY through its actual local GUI key binding."
  (should (eq (current-buffer) (window-buffer (selected-window))))
  (set-window-point (selected-window) (point))
  (should (commandp (key-binding (kbd key))))
  (when (equal key "TAB")
    (should (eq #'pi-coding-agent-toggle-tool-section (key-binding (kbd key))))
    (should (eq #'pi-coding-agent-toggle-tool-section (key-binding (kbd "<tab>")))))
  (execute-kbd-macro (kbd key))
  (redisplay t))

(defun pi-coding-agent-progress-gui-test--geometry (windows)
  "Return exact pixel edges, role and buffer identities of WINDOWS."
  (mapcar (lambda (window)
            (list window (window-pixel-edges window)
                  (window-parameter window 'cabins-agent-workspace-role)
                  (window-buffer window)))
          windows))

(defun pi-coding-agent-progress-gui-test--evidence (name windows &optional facts)
  "Record NAME's GUI assertions, WINDOWS and FACTS without desktop capture."
  (when pi-coding-agent-progress-gui-test--evidence-directory
    (let* ((directory pi-coding-agent-progress-gui-test--evidence-directory)
           (base (expand-file-name name directory))
           (export-status "Frame export unavailable: x-export-frames is not defined"))
      (redisplay t)
      (when (fboundp 'x-export-frames)
        (condition-case err
            (let ((data (x-export-frames (list (selected-frame)) 'png)))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert data)
                (let ((coding-system-for-write 'binary))
                  (write-region (point-min) (point-max) (concat base ".png") nil 'silent)))
              (setq export-status (concat base ".png")))
          (error (setq export-status (format "Frame export unsupported: %S" err)))))
      (with-temp-file (concat base ".el")
        (prin1
         (list :emacs-version emacs-version :window-system window-system
               :graphical (display-graphic-p) :frame-size
               (list (frame-width) (frame-height)) :frame-export export-status
               :limitations "No desktop screenshot or live user-runtime inspection; geometry is a synthetic Workbench-style layout, not parent init integration."
               :windows
               (mapcar (lambda (window)
                         (with-current-buffer (window-buffer window)
                           (list :role (window-parameter window 'cabins-agent-workspace-role)
                                 :buffer (buffer-name) :pixel-edges (window-pixel-edges window)
                                 :body-width (window-body-width window)
                                 :point (window-point window) :start (window-start window)
                                 :header (substring-no-properties
                                          (format-mode-line header-line-format nil window))
                                 :mode-line (substring-no-properties
                                             (format-mode-line mode-line-format nil window)))))
                       windows)
               :facts facts)
         (current-buffer))
        (insert "\n")))))

(defmacro pi-coding-agent-progress-gui-test--with-layout (&rest body)
  "Run BODY in a fresh synthetic Workbench/Output/Input graphical layout.
Bind chat, input, workbench, output-window, input-window, left-window,
windows and directory.  No process is created; only event fixtures are used."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (display-graphic-p))
     (should (fboundp 'pi-coding-agent--segmented-progress-p))
     (let* ((chat (generate-new-buffer " *pi-progress-gui-output*"))
            (input (generate-new-buffer " *pi-progress-gui-input*"))
            (workbench (generate-new-buffer " *pi-progress-gui-workbench*"))
            (directory (make-temp-file "/tmp/pi-progress-gui-fixture-" t))
            (frame (selected-frame))
            (old-width (frame-width frame))
            (old-height (frame-height frame)))
       (unwind-protect
           (save-window-excursion
             (set-frame-size frame 100 38)
             (delete-other-windows)
             (let* ((left-window (selected-window))
                    (output-window (split-window left-window 62 'right))
                    (input-window (split-window output-window -7 'below))
                    (windows (list left-window output-window input-window)))
               (set-window-buffer left-window workbench)
               (set-window-buffer output-window chat)
               (set-window-buffer input-window input)
               (cl-mapc (lambda (window role)
                          (set-window-parameter window 'cabins-agent-workspace-role role))
                        windows '(workbench agent-output agent-input))
               (with-current-buffer workbench
                 (insert "Synthetic Workbench\nNo user buffers or init loaded.\n"))
               (with-current-buffer chat
                 (pi-coding-agent-chat-mode)
                 (pi-coding-agent--set-chat-session-identity directory)
                 (setq-local pi-coding-agent--output-progress-display 'segmented)
                 (setq-local pi-coding-agent--tool-activity-display 'grouped)
                 (setq-local pi-coding-agent--thinking-activity-display 'grouped)
                 (setq-local pi-coding-agent--thinking-display 'hidden)
                 (setq-local pi-coding-agent--activity-icon-style 'text)
                 (setq-local pi-coding-agent--input-buffer input)
                 (setq-local pi-coding-agent--state
                             '(:model (:name "synthetic-model") :thinking-level "off"))
                 (setq-local pi-coding-agent--session-name "Synthetic GUI")
                 (setq-local pi-coding-agent--canonical-messages
                             (pi-coding-agent-progress-gui-test--history directory))
                 (pi-coding-agent--display-session-history
                  pi-coding-agent--canonical-messages chat)
                 (font-lock-ensure)
                 (should (pi-coding-agent--segmented-progress-p)))
               (with-current-buffer input
                 (pi-coding-agent-input-mode)
                 (setq-local pi-coding-agent--chat-buffer chat)
                 (insert "Unsent draft\nsecond line stays here"))
               (select-window input-window)
               (redisplay t)
               (should (<= (window-body-width output-window) 40))
               ,@body))
         (set-frame-size frame old-width old-height)
         (dolist (buffer (list chat input workbench))
           (when (buffer-live-p buffer) (kill-buffer buffer)))
         (delete-directory directory t)))))

(ert-deftest pi-coding-agent-gui-test-progress-tab-ret-exact-segment ()
  "Real TAB/RET resolve only the addressed segment and authoritative tool."
  (pi-coding-agent-progress-gui-test--with-layout
    (select-window output-window)
    (with-current-buffer chat
      (let* ((first (pi-coding-agent--activity-record-for-tool "gui-read"))
             (second (pi-coding-agent--activity-record-for-tool "gui-edit"))
             (messages (copy-tree pi-coding-agent--canonical-messages))
             (geometry (pi-coding-agent-progress-gui-test--geometry windows)))
        (should first)
        (should second)
        (should-not (eq first second))
        (should (eq first (pi-coding-agent--activity-record-for-tool "gui-bash")))
        (should (= 2 (pi-coding-agent--activity-record-calls first)))
        (should (= 1 (pi-coding-agent--activity-record-calls second)))
        (should-not (pi-coding-agent--activity-record-expanded first))
        (should-not (pi-coding-agent--activity-record-expanded second))
        (pi-coding-agent-progress-gui-test--goto-segment first)
        (pi-coding-agent-progress-gui-test--key "TAB")
        (should (pi-coding-agent--activity-record-expanded first))
        (should-not (pi-coding-agent--activity-record-expanded second))
        (should (string-match-p "\n    read" (pi-coding-agent-progress-gui-test--visible)))
        (should (string-match-p "\n    bash" (pi-coding-agent-progress-gui-test--visible)))
        (should-not (string-match-p "\n    edit" (pi-coding-agent-progress-gui-test--visible)))
        (pi-coding-agent-progress-gui-test--goto "    bash")
        (pi-coding-agent-progress-gui-test--key "TAB")
        (should (string-match-p "GUI_COMMAND_DETAIL_ALPHA"
                                (pi-coding-agent-progress-gui-test--visible)))
        (pi-coding-agent-progress-gui-test--goto "GUI_COMMAND_DETAIL_ALPHA")
        (pi-coding-agent-progress-gui-test--key "TAB")
        (should-not (string-match-p "GUI_COMMAND_DETAIL_ALPHA"
                                    (pi-coding-agent-progress-gui-test--visible)))
        ;; File visiting is the one intercepted boundary: verify the exact
        ;; canonical target, without introducing parent Workbench advice.
        (let (target)
          (cl-letf (((symbol-function 'pi-coding-agent--visit-file-target)
                     (lambda (value _toggle) (setq target value))))
            (pi-coding-agent-progress-gui-test--goto "    read")
            (pi-coding-agent-progress-gui-test--key "RET"))
          (should (equal (plist-get target :emacs-path)
                         (expand-file-name "authoritative-source-with-a-long-name.el" directory)))
          (should (= 7 (plist-get target :line))))
        (let (detail-id)
          (cl-letf (((symbol-function 'pi-coding-agent--open-tool-detail-buffer)
                     (lambda (id) (setq detail-id id))))
            (pi-coding-agent-progress-gui-test--goto "    bash")
            (pi-coding-agent-progress-gui-test--key "RET"))
          (should (equal "gui-bash" detail-id)))
        (pi-coding-agent-progress-gui-test--goto-segment second)
        (pi-coding-agent-progress-gui-test--key "RET")
        (should (pi-coding-agent--activity-record-expanded second))
        (should (pi-coding-agent--activity-record-expanded first))
        (should (string-match-p "\n    edit" (pi-coding-agent-progress-gui-test--visible)))
        (let (outline-called)
          (cl-letf (((symbol-function 'outline-cycle)
                     (lambda (&rest _) (setq outline-called t))))
            (pi-coding-agent-progress-gui-test--goto "Bridge B:")
            (forward-char 3)
            (pi-coding-agent-progress-gui-test--key "TAB"))
          (should outline-called)
          (should (pi-coding-agent--activity-record-expanded first))
          (should (pi-coding-agent--activity-record-expanded second)))
        (should (equal messages pi-coding-agent--canonical-messages))
        (should (equal geometry (pi-coding-agent-progress-gui-test--geometry windows)))
        (pi-coding-agent-progress-gui-test--evidence
         "segment-tab-ret" windows '(:exact-segment t :canonical-file-and-line t
                                     :single-tool-tab-and-ret t :body-outline t))))))

(ert-deftest pi-coding-agent-gui-test-progress-background-reading-anchor ()
  "Background end preserves expansion, historical viewport, draft and layout."
  (pi-coding-agent-progress-gui-test--with-layout
    (with-selected-window output-window
      (with-current-buffer chat
        ;; This live call starts in a new tool segment after the final body.
        (pi-coding-agent--handle-display-event '(:type "agent_start"))
        (pi-coding-agent--handle-display-event
         '(:type "tool_execution_start" :toolCallId "gui-background"
           :toolName "bash" :args (:command "synthetic background verification")))
        (let ((segment (pi-coding-agent--activity-record-for-tool "gui-background")))
          (should segment)
          (pi-coding-agent-progress-gui-test--goto-segment segment)
          (pi-coding-agent-progress-gui-test--key "TAB")
          (should (pi-coding-agent--activity-record-expanded segment))
          (pi-coding-agent-progress-gui-test--goto "Historical reading anchor 045")
          (set-window-point output-window (point))
          (forward-line -4)
          (set-window-start output-window (point) t)
          (goto-char (window-point output-window))
          (pi-coding-agent--viewport-reconcile-window output-window))))
    (select-window input-window)
    (redisplay t)
    (let* ((geometry (pi-coding-agent-progress-gui-test--geometry windows))
           (draft (with-current-buffer input (buffer-string)))
           (input-point (window-point input-window))
           (input-start (window-start input-window))
           (output-point (with-current-buffer chat
                           (copy-marker (window-point output-window))))
           (output-start (with-current-buffer chat
                           (copy-marker (window-start output-window))))
           (state (with-current-buffer chat
                    (pi-coding-agent--viewport-window-state output-window)))
           (segment (with-current-buffer chat
                      (pi-coding-agent--activity-record-for-tool "gui-background"))))
      (should (eq 'reading (pi-coding-agent--viewport-state-mode state)))
      (unwind-protect
          (progn
            (with-current-buffer chat
              (pi-coding-agent--handle-display-event
               '(:type "tool_execution_update" :toolCallId "gui-background"
                 :partialResult (:content [(:type "text" :text "partial synthetic result")]))))
            (with-current-buffer chat
              (pi-coding-agent--handle-display-event
               '(:type "tool_execution_end" :toolCallId "gui-background" :toolName "bash"
                 :result (:content [(:type "text" :text "Error: background synthetic failure")]) :isError t)))
            (redisplay t)
            (should (eq input-window (selected-window)))
            (should (equal geometry (pi-coding-agent-progress-gui-test--geometry windows)))
            (should (= output-point (window-point output-window)))
            (should (= output-start (window-start output-window)))
            (should (= input-point (window-point input-window)))
            (should (= input-start (window-start input-window)))
            (should (equal draft (with-current-buffer input (buffer-string))))
            (should (eq 'reading (pi-coding-agent--viewport-state-mode state)))
            (should (pi-coding-agent--activity-record-expanded segment))
            (should (= 1 (pi-coding-agent--activity-record-failed segment)))
            (should (= 1 (pi-coding-agent--activity-record-completed segment)))
            (with-current-buffer chat
              (should-not (string-match-p "working" (pi-coding-agent-progress-gui-test--visible)))
              (should (string-match-p "\n  ! bash.*failed"
                                      (pi-coding-agent-progress-gui-test--visible)))
              (should (eq 'failed (plist-get (car (pi-coding-agent--activity-record-tools
                                                   segment)) :status))))
            (pi-coding-agent-progress-gui-test--evidence
             "background-reading" windows
             '(:selected-input-unchanged t :historical-point-and-start t
               :manual-expansion-kept t :failed-late-end-counted t :draft-and-geometry t)))
        (set-marker output-point nil)
        (set-marker output-start nil)))))

(ert-deftest pi-coding-agent-gui-test-progress-fixed-state-routing ()
  "Cached state uses Input's header or Output's mode-line, never both."
  (pi-coding-agent-progress-gui-test--with-layout
    (let ((clock 100.0)
          ;; Mocking native subrs must not schedule Emacs's own deferred
          ;; trampoline compiler while progress timer guards are active.
          (native-comp-enable-subr-trampolines nil)
          (draft (with-current-buffer input (buffer-string))))
      (with-current-buffer chat
        ;; Replayed canonical timestamps must not synthesize elapsed time.
        (should-not (pi-coding-agent--progress-elapsed))
        (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock))
                  ((symbol-function 'pi-coding-agent--refresh-header)
                   (lambda (&rest _) (ert-fail "Progress fetched session stats")))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _) (ert-fail "Progress scheduled a timer"))))
          (pi-coding-agent--progress-start)
          (pi-coding-agent--set-activity-phase "thinking")
          (setq clock 100.5)
          (pi-coding-agent--update-progress
           '(:calls 3 :started 3 :completed 1 :running 2 :failed 1)
           "DO-NOT-CLAIM-ONE-PARALLEL-TOOL")
          (should (= 0.5 (pi-coding-agent--progress-elapsed)))))
      (with-selected-window input-window
        (let* ((header (pi-coding-agent--header-line-string))
               (full (or (get-text-property 0 'help-echo header) header)))
          (should header-line-format)
          (should (string-match-p "!1.*running\\|running.*!1" header))
          (should (<= (string-width header) (window-body-width input-window)))
          (should-not (string-match-p "[\n\r]" header))
          (should (string-match-p "2 running" full))
          (should (string-match-p "1/3 calls" full))
          (should (string-match-p "0\\.5s" full))
          (should (string-match-p "synthetic-model" full))
          (should (string-match-p "Synthetic GUI" full))))
      (with-selected-window output-window
        (should-not (pi-coding-agent--output-progress-mode-line-string))
        (should-not header-line-format)
        (should-not (string-match-p "1/3 calls"
                                    (format-mode-line mode-line-format))))
      (pi-coding-agent-progress-gui-test--evidence
       "input-header-progress" windows '(:input-header-only t :parallel-count t
                                         :failure-priority t :history-no-elapsed t))
      ;; Deliberately remove Input, as a user would.  The progress renderer
      ;; must not reopen it, create a status window, or alter the new geometry.
      (delete-window input-window)
      (setq windows (list left-window output-window))
      (select-window left-window)
      (redisplay t)
      (let ((geometry (pi-coding-agent-progress-gui-test--geometry windows))
            (before (with-current-buffer chat (buffer-string)))
            (cache (buffer-local-value 'pi-coding-agent--progress-cache chat)))
        ;; Only display evaluation is under these guards: no RPC, file reads,
        ;; shell, clock sampling, history scans, new timer or buffer mutation.
        (cl-letf (((symbol-function 'float-time)
                   (lambda (&rest _) (ert-fail "Display sampled elapsed time")))
                  ((symbol-function 'pi-coding-agent--rpc-async)
                   (lambda (&rest _) (ert-fail "Display requested RPC")))
                  ((symbol-function 'pi-coding-agent--refresh-header)
                   (lambda (&rest _) (ert-fail "Display fetched session stats")))
                  ((symbol-function 'insert-file-contents)
                   (lambda (&rest _) (ert-fail "Display read a file")))
                  ((symbol-function 'process-file)
                   (lambda (&rest _) (ert-fail "Display ran a subprocess")))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _) (ert-fail "Display scheduled a timer")))
                  ((symbol-function 'pi-coding-agent--collect-you-headings)
                   (lambda (&rest _) (ert-fail "Display scanned history"))))
          (with-selected-window output-window
            (dotimes (_ 5)
              (let ((status (pi-coding-agent--output-progress-mode-line-string)))
                (should (string-match-p "!1.*running\\|running.*!1" status))
                (should-not (string-match-p "[\n\r]" status))
                (should (string-match-p "!1.*running\\|running.*!1"
                                        (format-mode-line mode-line-format)))
                (should-not header-line-format)))))
        (should (eq left-window (selected-window)))
        (should (equal geometry (pi-coding-agent-progress-gui-test--geometry windows)))
        (should (equal before (with-current-buffer chat (buffer-string))))
        (should (equal cache (buffer-local-value 'pi-coding-agent--progress-cache chat)))
        (should (equal draft (with-current-buffer input (buffer-string))))
        (should (= 2 (length (window-list))))
        (pi-coding-agent-progress-gui-test--evidence
         "output-only-mode-line" windows '(:mode-line-only t :cached-read-only t
                                           :no-new-window t :draft-kept t)))
      (with-current-buffer chat
        (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock)))
          (setq clock 103.5)
          (pi-coding-agent--progress-finish "done")
          (should (= 3.5 (pi-coding-agent--progress-elapsed)))
          (setq clock 999.0)
          (pi-coding-agent--update-progress
           '(:calls 3 :completed 3 :running 0 :failed 1))
          (should (= 3.5 (pi-coding-agent--progress-elapsed)))
          (should (string-match-p "!1.*done\\|done.*!1" (pi-coding-agent--progress-string)))
          (should-not (string-match-p "running" (pi-coding-agent--progress-string))))))))

(ert-deftest pi-coding-agent-gui-test-progress-shared-live-fixture ()
  "Adversarial canonical fixture retains segment order in a narrow GUI."
  (pi-coding-agent-progress-gui-test--with-layout
    (let* ((fixture (pi-coding-agent-progress-fixture))
           (draft (with-current-buffer input (buffer-string)))
           (geometry (pi-coding-agent-progress-gui-test--geometry windows)))
      (with-current-buffer chat
        (dolist (event (plist-get fixture :events))
          (pi-coding-agent--handle-display-event event))
        (let* ((first (pi-coding-agent--activity-record-for-tool "segment-1"))
               (second (pi-coding-agent--activity-record-for-tool "segment-3"))
               (turn (pi-coding-agent--activity-record-for-turn
                      (pi-coding-agent--activity-record-turn-id first)))
               (text (pi-coding-agent-progress-gui-test--visible)))
          (should (eq first (pi-coding-agent--activity-record-for-tool "segment-2")))
          (should-not (eq first second))
          (should (= 2 (length (pi-coding-agent--activity-record-segments turn))))
          (should (= 2 (pi-coding-agent--activity-record-completed first)))
          (should (= 1 (pi-coding-agent--activity-record-completed second)))
          (should (< (string-match "Body A" text)
                     (marker-position (pi-coding-agent--activity-record-start-marker first))))
          (should (< (marker-position (pi-coding-agent--activity-record-end-marker first))
                     (marker-position (pi-coding-agent--activity-record-start-marker second))))
          (should (string-match-p "Body B" text))
          (should (string-match-p "Body C" text))
          (should-not (string-match-p "working\\|PRIVATE PAYLOAD" text))
          (dolist (segment (list first second))
            (save-excursion
              (goto-char (pi-coding-agent--activity-record-start-marker segment))
              (should (<= (string-width (buffer-substring-no-properties
                                        (line-beginning-position) (line-end-position)))
                          (window-body-width output-window)))))))
      (redisplay t)
      (should (eq input-window (selected-window)))
      (should (equal draft (with-current-buffer input (buffer-string))))
      (should (equal geometry (pi-coding-agent-progress-gui-test--geometry windows)))
      (pi-coding-agent-progress-gui-test--evidence
       "shared-live-fixture" windows '(:delayed-starts-original-segment t
                                       :duplicates-counted-once t :narrow-lines t)))))

(ert-deftest pi-coding-agent-gui-test-progress-mode-line-evaluated-frame ()
  "Output status routes by evaluated frame, not another selected frame."
  (pi-coding-agent-progress-gui-test--with-layout
    (let* ((origin-frame (selected-frame))
           (other-frame (make-frame '((name . "Isolated progress secondary frame")
                                      (no-focus-on-map . t) (no-accept-focus . t)
                                      (left . 10000) (top . 10000)
                                      (width . 44) (height . 20))))
           (other-window (frame-selected-window other-frame)))
      (unwind-protect
          (progn
            (set-window-buffer other-window chat)
            (with-current-buffer chat
              (pi-coding-agent--set-activity-phase "running")
              (pi-coding-agent--update-progress '(:calls 2 :completed 1 :running 1)))
            (select-frame origin-frame)
            (select-window input-window)
            (with-current-buffer chat
              (let ((mode-line-window other-window))
                (should (string-match-p "running"
                                        (pi-coding-agent--output-progress-mode-line-string))))
              (let ((mode-line-window output-window))
                (should-not (pi-coding-agent--output-progress-mode-line-string)))
              (should (string-match-p "running"
                                      (format-mode-line mode-line-format nil other-window))))
            (should (eq origin-frame (selected-frame)))
            (should (eq input-window (selected-window)))
            (should (= 3 (length (window-list origin-frame))))
            (should (= 1 (length (window-list other-frame))))
            (pi-coding-agent-progress-gui-test--evidence
             "evaluated-frame-routing" windows '(:same-chat-two-frames t
                                                  :selected-frame-unchanged t)))
        (delete-frame other-frame t)))))

(provide 'pi-coding-agent-progress-gui-test)
;;; pi-coding-agent-progress-gui-test.el ends here
