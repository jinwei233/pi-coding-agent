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

(ert-deftest pi-coding-agent-test-model-switching-sends-explicit-scope ()
  "Model switching sends explicit current-only and sticky scope values."
  (let ((chat-buf (generate-new-buffer "*pi-model-scope-test*"))
        commands)
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
                                         :name "DeepSeek V4 Pro"))))))
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
          (should (eq (plist-get (car commands) :persistDefault)
                      :json-false))
          (should (eq (plist-get (cadr commands) :persistDefault) t)))
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
  "A failed model RPC leaves the current buffer model unchanged."
  (let ((chat-buf (generate-new-buffer "*pi-model-failure-test*"))
        last-message)
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
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq last-message
                             (apply #'format format-string args)))))
            (pi-coding-agent--switch-model
             :proc chat-buf
             '(:provider "deepseek" :id "deepseek-v4-pro"
               :name "DeepSeek V4 Pro")
             nil))
          (should
           (equal
            (with-current-buffer chat-buf
              (plist-get (plist-get pi-coding-agent--state :model) :id))
            "deepseek-v4-flash"))
          (should (string-match-p "quota exceeded" last-message)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-model-switching-helm-has-scope-actions ()
  "The Helm source exposes current-only and sticky actions in that order."
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
              '("Switch current Session"
                "Switch Session and set new-session default")))
      (funcall (cdar captured-actions)
               '(:provider "deepseek" :id "deepseek-v4-pro"))
      (funcall (cdadr captured-actions)
               '(:provider "deepseek" :id "deepseek-v4-pro"))
      (should
       (equal (nreverse dispatched)
              '(("deepseek-v4-pro" nil)
                ("deepseek-v4-pro" t)))))))

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
