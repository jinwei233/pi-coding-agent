;;; pi-coding-agent-model-switching-test.el --- Model switching tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'pi-coding-agent)

(ert-deftest pi-coding-agent-test-model-switching-filters-runtime-catalog ()
  "Runtime candidates obey the configured model allowlist."
  (let ((pi-coding-agent-model-allowlist
         '(("deepseek" . "deepseek-v4-flash")
           ("deepseek" . "deepseek-v4-pro"))))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-sync)
               (lambda (&rest _)
                 '(:success t
                   :data
                   (:models
                    [(:provider "deepseek" :id "deepseek-v4-flash")
                     (:provider "deepseek" :id "deepseek-v4-pro")
                     (:provider "anthropic" :id "claude-opus")])))))
      (should
       (equal
        (mapcar #'pi-coding-agent--model-reference
                (pi-coding-agent--available-switch-models :proc))
        '(("deepseek" . "deepseek-v4-flash")
          ("deepseek" . "deepseek-v4-pro")))))))

(defun pi-coding-agent-test--parse-settings (path)
  "Parse settings PATH with stable JSON sentinels and string top-level keys."
  (mapcar
   (lambda (entry)
     (cons (if (symbolp (car entry))
               (symbol-name (car entry))
             (car entry))
           (cdr entry)))
   (json-parse-string
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))
    :object-type 'alist :array-type 'array
    :null-object :null :false-object :false)))

(ert-deftest pi-coding-agent-test-settings-persistence-creates-missing-directory ()
  "Persistence creates a missing agent directory and settings file."
  (let* ((root (make-temp-file "pi-settings-create-" t))
         (path (expand-file-name "missing/agent/settings.json" root)))
    (unwind-protect
        (progn
          (should (eq (plist-get
                       (pi-coding-agent--update-default-model-settings
                        path "deepseek" "pro")
                       :status)
                      'written))
          (should
           (equal (pi-coding-agent-test--parse-settings path)
                  '(("defaultProvider" . "deepseek")
                    ("defaultModel" . "pro")))))
      (delete-directory root t))))

(ert-deftest pi-coding-agent-test-settings-persistence-preserves-unrelated-values ()
  "Persistence keeps unrelated keys and JSON value types intact."
  (let* ((root (make-temp-file "pi-settings-values-" t))
         (path (expand-file-name "settings.json" root))
         (original
          '(("packages" . ["one"])
            ("nothing" . :null)
            ("disabled" . :false)
            ("count" . 42)
            ("label" . "中文"))))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert (pi-coding-agent--encode-settings-json original)))
          (should (eq (plist-get
                       (pi-coding-agent--update-default-model-settings
                        path "deepseek" "pro")
                       :status)
                      'written))
          (let ((written (pi-coding-agent-test--parse-settings path)))
            (dolist (entry original)
              (should (equal (assoc (car entry) written) entry)))))
      (delete-directory root t))))

(ert-deftest pi-coding-agent-test-settings-persistence-complete-state-is-byte-noop ()
  "Complete defaults and exact scope do not rewrite the file."
  (let* ((root (make-temp-file "pi-settings-noop-" t))
         (path (expand-file-name "settings.json" root))
         (contents
          "{\n  \"defaultProvider\": \"DeepSeek\",\n  \"defaultModel\": \"Pro\",\n  \"enabledModels\": [\"DEEPSEEK/PRO\"]\n}\n"))
    (unwind-protect
        (progn
          (with-temp-file path (insert contents))
          (should (eq (plist-get
                       (pi-coding-agent--update-default-model-settings
                        path "DeepSeek" "Pro")
                       :status)
                      'noop))
          (should (equal (with-temp-buffer
                           (insert-file-contents-literally path)
                           (buffer-string))
                         contents)))
      (delete-directory root t))))

(ert-deftest pi-coding-agent-test-settings-persistence-repairs-incomplete-scope ()
  "Matching defaults still append a missing exact enabledModels entry."
  (let* ((root (make-temp-file "pi-settings-scope-" t))
         (path (expand-file-name "settings.json" root)))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"defaultProvider\":\"deepseek\",\"defaultModel\":\"pro\",\"enabledModels\":[\"deepseek/*\"]}"))
          (should (eq (plist-get
                       (pi-coding-agent--update-default-model-settings
                        path "deepseek" "pro")
                       :status)
                      'written))
          (should (equal (cdr (assoc "enabledModels"
                                     (pi-coding-agent-test--parse-settings path)))
                         ["deepseek/*" "deepseek/pro"]))
          (should (eq (plist-get
                       (pi-coding-agent--update-default-model-settings
                        path "deepseek" "pro")
                       :status)
                      'noop)))
      (delete-directory root t))))

(ert-deftest pi-coding-agent-test-settings-persistence-rejects-unsafe-json ()
  "Malformed, non-object, and invalid enabledModels settings stay unchanged."
  (dolist (contents '("{broken" "[]" "null"
                      "{\"enabledModels\":\"deepseek/*\"}"))
    (let* ((root (make-temp-file "pi-settings-invalid-" t))
           (path (expand-file-name "settings.json" root)))
      (unwind-protect
          (progn
            (with-temp-file path (insert contents))
            (should (eq (plist-get
                         (pi-coding-agent--update-default-model-settings
                          path "deepseek" "pro")
                         :status)
                        'failed))
            (should (equal (with-temp-buffer
                             (insert-file-contents-literally path)
                             (buffer-string))
                           contents)))
        (delete-directory root t)))))

(ert-deftest pi-coding-agent-test-settings-persistence-accepts-bom ()
  "A UTF-8 BOM is ignored while parsing existing settings."
  (let* ((root (make-temp-file "pi-settings-bom-" t))
         (path (expand-file-name "settings.json" root)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-with-signature))
            (write-region "{\"other\":true}" nil path nil 'silent))
          (should (eq (plist-get
                       (pi-coding-agent--update-default-model-settings
                        path "deepseek" "pro")
                       :status)
                      'written))
          (should (eq (cdr (assoc "other"
                                  (pi-coding-agent-test--parse-settings path)))
                      t)))
      (delete-directory root t))))

(ert-deftest pi-coding-agent-test-settings-persistence-preserves-mode-and-symlink ()
  "Atomic replacement keeps target mode and does not replace a symlink."
  (let* ((root (make-temp-file "pi-settings-link-" t))
         (target (expand-file-name "managed-settings.json" root))
         (link (expand-file-name "settings.json" root)))
    (unwind-protect
        (progn
          (with-temp-file target (insert "{\"other\":1}"))
          (set-file-modes target #o644)
          (make-symbolic-link target link)
          (should (eq (plist-get
                       (pi-coding-agent--update-default-model-settings
                        link "deepseek" "pro")
                       :status)
                      'written))
          (should (file-symlink-p link))
          (should (= (file-modes target) #o644))
          (should (equal (cdr (assoc "defaultModel"
                                     (pi-coding-agent-test--parse-settings target)))
                         "pro")))
      (delete-directory root t))))

(ert-deftest pi-coding-agent-test-settings-persistence-cleans-temp-after-rename-failure ()
  "A failed rename reports failure and removes its temporary file."
  (let* ((root (make-temp-file "pi-settings-rename-" t))
         (path (expand-file-name "settings.json" root))
         temporary)
    (unwind-protect
        (progn
          (with-temp-file path (insert "{}"))
          (cl-letf (((symbol-function 'rename-file)
                     (lambda (source &rest _)
                       (setq temporary source)
                       (error "simulated rename failure"))))
            (should (eq (plist-get
                         (pi-coding-agent--update-default-model-settings
                          path "deepseek" "pro")
                         :status)
                        'failed)))
          (should temporary)
          (should-not (file-exists-p temporary))
          (should (equal (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         "{}")))
      (delete-directory root t))))

(ert-deftest pi-coding-agent-test-model-switching-sends-documented-rpc-only ()
  "Both actions send only documented set_model fields over RPC."
  (let ((chat-buf (generate-new-buffer "*pi-model-scope-test*"))
        commands
        persisted)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle
                  pi-coding-agent--state
                  '(:model (:provider "deepseek"
                            :id "deepseek-v4-flash"
                            :name "DeepSeek V4 Flash"))))
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc command callback)
                       (push command commands)
                       (funcall callback
                                '(:success t
                                  :command "set_model"
                                  :data (:provider "deepseek"
                                         :id "deepseek-v4-pro"
                                         :name "DeepSeek V4 Pro")))))
                    ((symbol-function 'pi-coding-agent--persist-default-model)
                     (lambda (&rest args)
                       (setq persisted args)
                       '(:status noop :path "/tmp/settings.json"))))
            (pi-coding-agent--switch-model
             :proc chat-buf
             '(:provider "deepseek" :id "deepseek-v4-pro"
               :name "DeepSeek V4 Pro")
             nil)
            (pi-coding-agent--switch-model
             :proc chat-buf
             '(:provider "deepseek" :id "deepseek-v4-pro"
               :name "DeepSeek V4 Pro")
             t))
          (setq commands (nreverse commands))
          (dolist (command commands)
            (should (equal command
                           '(:type "set_model"
                             :provider "deepseek"
                             :modelId "deepseek-v4-pro"))))
          (should (equal persisted
                         '(:proc "deepseek" "deepseek-v4-pro"))))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-model-switching-rejects-busy-dispatch ()
  "A busy Session rejects switching before sending RPC."
  (let ((chat-buf (generate-new-buffer "*pi-model-busy-test*"))
        rpc-called)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming))
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (&rest _)
                       (setq rpc-called t))))
            (should-error
             (pi-coding-agent--switch-model
              :proc chat-buf
              '(:provider "deepseek" :id "deepseek-v4-pro")
              nil)
             :type 'user-error))
          (should-not rpc-called))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-model-switching-failure-preserves-model ()
  "A failed model RPC leaves the model unchanged and never persists."
  (let ((chat-buf (generate-new-buffer "*pi-model-failure-test*"))
        last-message
        persisted)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle
                  pi-coding-agent--state
                  '(:model (:provider "deepseek"
                            :id "deepseek-v4-flash"
                            :name "DeepSeek V4 Flash"))))
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _command callback)
                       (funcall callback
                                '(:success :false :error "quota exceeded"))))
                    ((symbol-function 'pi-coding-agent--persist-default-model)
                     (lambda (&rest _)
                       (setq persisted t)))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq last-message
                             (apply #'format format-string args)))))
            (pi-coding-agent--switch-model
             :proc chat-buf
             '(:provider "deepseek" :id "deepseek-v4-pro"
               :name "DeepSeek V4 Pro")
             t))
          (should
           (equal
            (with-current-buffer chat-buf
              (plist-get (plist-get pi-coding-agent--state :model) :id))
            "deepseek-v4-flash"))
          (should (string-match-p "quota exceeded" last-message))
          (should-not persisted))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-model-switching-helm-has-scope-actions ()
  "The Helm source exposes sticky and current-only actions in that order."
  (let (captured-actions dispatched)
    (cl-letf (((symbol-function 'helm-make-source)
               (lambda (_name _class &rest args)
                 (setq captured-actions (plist-get args :action))
                 args))
              ((symbol-function 'helm)
               (lambda (&rest _)))
              ((symbol-function 'pi-coding-agent--switch-model)
               (lambda (_proc _chat model persist-default)
                 (push (list (plist-get model :id) persist-default)
                       dispatched))))
      (pi-coding-agent--show-model-helm
       :proc :chat
       '(("Pro" . (:provider "deepseek" :id "deepseek-v4-pro")))
       nil)
      (should
       (equal (mapcar #'car captured-actions)
              '("Switch and remember as default"
                "Just this session")))
      (funcall (cdar captured-actions)
               '(:provider "deepseek" :id "deepseek-v4-pro"))
      (funcall (cdadr captured-actions)
               '(:provider "deepseek" :id "deepseek-v4-pro"))
      (should
       (equal (nreverse dispatched)
              '(("deepseek-v4-pro" t)
                ("deepseek-v4-pro" nil)))))))

(ert-deftest pi-coding-agent-test-model-switching-completion-is-sticky ()
  "The non-Helm completion fallback uses switch-and-remember semantics."
  (let (persist-default)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Pro"))
              ((symbol-function 'pi-coding-agent--switch-model)
               (lambda (_proc _chat _model persist)
                 (setq persist-default persist))))
      (pi-coding-agent--show-model-completion
       :proc :chat
       '(("Pro" . (:provider "deepseek" :id "pro")))
       '(:name "Flash") nil)
      (should persist-default))))

(ert-deftest pi-coding-agent-test-model-switching-persistence-failure-warns-after-switch ()
  "Persistence failure warns without undoing a successful Session switch."
  (let ((chat-buf (generate-new-buffer "*pi-model-persist-failure-test*"))
        warning)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle
                  pi-coding-agent--state
                  '(:model (:provider "deepseek" :id "flash"))))
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _command callback)
                       (funcall callback
                                '(:success t :command "set_model"
                                  :data (:provider "deepseek" :id "pro")))))
                    ((symbol-function 'pi-coding-agent--persist-default-model)
                     (lambda (&rest _)
                       '(:status failed :path "/tmp/settings.json"
                         :reason "invalid JSON")))
                    ((symbol-function 'display-warning)
                     (lambda (_type text &rest _)
                       (setq warning text)))
                    ((symbol-function 'message) #'ignore))
            (pi-coding-agent--switch-model
             :proc chat-buf '(:provider "deepseek" :id "pro") t))
          (should (equal
                   (with-current-buffer chat-buf
                     (plist-get (plist-get pi-coding-agent--state :model) :id))
                   "pro"))
          (should (string-match-p "/tmp/settings.json" warning))
          (should (string-match-p "invalid JSON" warning)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-model-switching-shows-price-without-tier-label ()
  "Candidates and Header avoid redundant tier labels."
  (let* ((model '(:provider "deepseek"
                  :id "deepseek-v4-pro"
                  :name "DeepSeek V4 Pro"
                  :cost (:input 0.28 :cacheRead 0.0028 :output 0.42)))
         (candidate (pi-coding-agent--model-candidate-label model nil))
         (chat-buf (generate-new-buffer "*pi-model-header-test*"))
         (input-buf (generate-new-buffer "*pi-model-header-input-test*")))
    (unwind-protect
        (progn
          (should-not (string-match-p "premium" candidate))
          (should (string-match-p "\\$0.28/M miss" candidate))
          (should (string-match-p "\\$0.0028/M cache" candidate))
          (should (string-match-p "V4 Pro" candidate))
          (should (string-match-p "\\[ds/v4-pro\\]" candidate))
          (should-not (string-match-p "DeepSeek V4 Pro" candidate))
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--state (list :model model)
                  pi-coding-agent--activity-phase "idle"))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (should-not
             (string-match-p
              "premium"
              (substring-no-properties
               (pi-coding-agent--header-line-string))))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-model-display-shortens-deepseek-vision ()
  "DeepSeek Vision names stay compact in Header and model candidates."
  (let* ((model '(:provider "deepseek"
                  :id "deepseek-v4-flash-vision-exp"
                  :name "DeepSeek V4 Flash Vision Exp"
                  :cost (:input 0.22 :cacheRead 0.007 :output 0.66)))
         (candidate (pi-coding-agent--model-candidate-label model model))
         (chat-buf (generate-new-buffer "*pi-model-vision-header-test*"))
         (input-buf (generate-new-buffer "*pi-model-vision-header-input-test*")))
    (unwind-protect
        (progn
          (should (string-match-p "\\*  V4 Vision" candidate))
          (should (string-match-p "\\[ds/v4-vision\\]" candidate))
          (should-not (string-match-p "DeepSeek V4 Flash Vision Exp" candidate))
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--state (list :model model)
                  pi-coding-agent--activity-phase "idle"))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (let ((header (substring-no-properties
                           (pi-coding-agent--header-line-string))))
              (should (string-match-p "V4 Vision" header))
              (should-not
               (string-match-p "DeepSeek V4 Flash Vision Exp" header)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-model-prices-support-cny ()
  "Model and session costs convert from USD to configured CNY."
  (let* ((pi-coding-agent-price-currency 'cny)
         (pi-coding-agent-usd-to-cny-rate 7.2)
         (model '(:provider "deepseek"
                  :id "deepseek-v4-flash"
                  :name "DeepSeek V4 Flash"
                  :cost (:input 0.14 :cacheRead 0.0028 :output 0.28)))
         (candidate (pi-coding-agent--model-candidate-label model nil))
         (stats '(:tokens (:input 1 :output 2 :total 3
                           :cacheRead 0 :cacheWrite 0)
                  :cost 0.435 :userMessages 1 :toolCalls 0)))
    (should (string-match-p "≈¥1.01/M miss" candidate))
    (should (string-match-p "≈¥0.02/M cache" candidate))
    (should (string-match-p "≈¥2.02/M out" candidate))
    (should (string-match-p "Session estimate: ≈¥3.13"
                            (pi-coding-agent--format-session-stats stats)))
    (should (equal (pi-coding-agent--format-cost 0.87 2) "≈¥6.26"))))

(ert-deftest pi-coding-agent-test-model-candidates-align-columns ()
  "Model candidates align identity, input price, and output price."
  (let* ((pi-coding-agent-price-currency 'cny)
         (pi-coding-agent-usd-to-cny-rate 7.2)
         (models
          '((:provider "deepseek" :id "deepseek-v4-flash"
             :name "DeepSeek V4 Flash"
             :cost (:input 0.14 :cacheRead 0.0028 :output 0.28))
            (:provider "deepseek" :id "deepseek-v4-pro"
             :name "DeepSeek V4 Pro"
             :cost (:input 0.435 :cacheRead 0.003625 :output 0.87))))
         (labels
          (mapcar #'car
                  (pi-coding-agent--model-candidates
                   models (car models))))
         (first (car labels))
         (second (cadr labels)))
    (should (= (string-match-p "\\[" first)
               (string-match-p "\\[" second)))
    (should (= (string-match-p "≈¥" first)
               (string-match-p "≈¥" second)))
    (should (= (string-match-p "≈¥[0-9.]+/M cache" first)
               (string-match-p "≈¥[0-9.]+/M cache" second)))
    (should (= (string-match-p "≈¥[0-9.]+/M out" first)
               (string-match-p "≈¥[0-9.]+/M out" second)))))

(provide 'pi-coding-agent-model-switching-test)
;;; pi-coding-agent-model-switching-test.el ends here
