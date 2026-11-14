;;; pi-coding-agent-progress-fixture.el --- Shared progress fixture -*- lexical-binding: t; -*-

;;; Commentary:

;; One canonical A -> tools 1/2 -> B -> tool 3 -> C transcript and its live
;; event stream.  Execution starts are deliberately delayed past B and results
;; are duplicated/late.  No display-specific synthetic history events are used.

;;; Code:

(require 'cl-lib)

(defun pi-coding-agent-progress-fixture ()
  "Return fresh canonical messages and matching adversarial live events."
  (let* ((path "/tmp/pi-tool-activity/shared.el")
         (call1 (list :type "toolCall" :id "segment-1" :name "edit"
                      :arguments (list :path path)))
         (call2 '(:type "toolCall" :id "segment-2" :name "remote_query"
                  :metadata (:toolType "mcp") :arguments (:query "lookup")))
         (call3 (list :type "toolCall" :id "segment-3" :name "edit"
                      :arguments (list :path path)))
         (blocks (vector '(:type "text" :text "Body A\n")
                         '(:type "thinking" :thinking "thought one")
                         call1 '(:type "text" :text " \n")
                         '(:type "thinking" :thinking "thought two")
                         call2 '(:type "text" :text "Body B\n")
                         '(:type "thinking" :thinking "thought three")
                         call3 '(:type "text" :text "Body C\n")))
         (assistant (list :role "assistant" :content blocks :stopReason "stop"))
         (result1 '(:role "toolResult" :toolCallId "segment-1" :toolName "edit"
                    :content [(:type "text" :text "PRIVATE PAYLOAD one")]
                    :details (:diff "+ 1 new\n- 1 old")))
         (result2 '(:role "toolResult" :toolCallId "segment-2" :toolName "remote_query"
                    :content [(:type "text" :text "PRIVATE PAYLOAD two")]))
         (result3 '(:role "toolResult" :toolCallId "segment-3" :toolName "edit"
                    :content [(:type "text" :text "PRIVATE PAYLOAD three")]
                    :details (:diff "+ 1 newer\n- 1 new")))
         (messages (vector '(:role "user" :content "segmented fixture")
                           assistant result3 result1 result2))
         events)
    (cl-labels
        ((event (value) (push value events))
         (update (type &rest properties)
           (event (list :type "message_update"
                        :message (list :role "assistant" :content
                                       (cl-subseq blocks 0
                                                  (1+ (or (plist-get properties :contentIndex) 0))))
                        :assistantMessageEvent (cons :type (cons type properties)))))
         (start (call)
           (event (list :type "tool_execution_start" :toolCallId (plist-get call :id)
                        :toolName (plist-get call :name) :args (plist-get call :arguments))))
         (end (result)
           (event (list :type "tool_execution_end" :toolCallId (plist-get result :toolCallId)
                        :toolName (plist-get result :toolName)
                        :result (list :content (plist-get result :content)
                                      :details (plist-get result :details))))))
      (event '(:type "message_start" :message (:role "user" :content [(:type "text" :text "segmented fixture")])))
      (event '(:type "agent_start"))
      (event '(:type "message_start" :message (:role "assistant" :content [])))
      (dotimes (index (length blocks))
        (let ((block (aref blocks index)))
          (pcase (plist-get block :type)
            ("text"
             (update "text_start" :contentIndex index)
             ;; B arrives as consecutive deltas: close only once.
             (if (= index 6)
                 (progn (update "text_delta" :contentIndex index :delta "Body ")
                        (update "text_delta" :contentIndex index :delta "B\n"))
               (update "text_delta" :contentIndex index :delta (plist-get block :text)))
             (update "text_end" :contentIndex index))
            ("thinking"
             (update "thinking_start" :contentIndex index)
             (update "thinking_delta" :contentIndex index :delta (plist-get block :thinking))
             (update "thinking_end" :contentIndex index :content (plist-get block :thinking)))
            ("toolCall"
             (update "toolcall_start" :contentIndex index)
             (update "toolcall_end" :contentIndex index)
             (when (= index 8)
               ;; Starts for the first two calls arrive after B and tool 3.
               (start call1) (start call1) (start call2) (start call3)
               (end result3) (end result1) (end result1)
               ;; An even later duplicate start cannot downgrade completion.
               (start call1) (end result2) (end result2))))))
      (event (list :type "message_end" :message assistant))
      (event (list :type "agent_end" :messages messages)))
    (list :messages messages :events (nreverse events))))

(provide 'pi-coding-agent-progress-fixture)
;;; pi-coding-agent-progress-fixture.el ends here
