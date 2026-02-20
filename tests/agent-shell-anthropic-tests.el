;;; agent-shell-anthropic-tests.el --- Tests for agent-shell-anthropic -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-anthropic)

(ert-deftest agent-shell-anthropic-make-claude-client-test ()
  "Test agent-shell-anthropic-make-claude-client function."
  ;; Mock executable-find to always return the command path
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) "/usr/bin/claude-agent-acp")))
    ;; Test with API key authentication
    (let* ((agent-shell-anthropic-authentication '(:api-key "test-api-key"))
           (agent-shell-anthropic-claude-acp-command '("claude-agent-acp" "--json"))
           (agent-shell-anthropic-claude-environment '("DEBUG=1"))
           (test-buffer (get-buffer-create "*test-buffer*"))
           (client (agent-shell-anthropic-make-claude-client :buffer test-buffer)))
      (unwind-protect
          (progn
            (should (listp client))
            (should (equal (map-elt client :command) "claude-agent-acp"))
            (should (equal (map-elt client :command-params) '("--json")))
            (should (member "ANTHROPIC_API_KEY=test-api-key" (map-elt client :environment-variables)))
            (should (member "DEBUG=1" (map-elt client :environment-variables))))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))

    ;; Test with login authentication
    (let* ((agent-shell-anthropic-authentication '(:login t))
           (agent-shell-anthropic-claude-acp-command '("claude-agent-acp" "--interactive"))
           (agent-shell-anthropic-claude-environment '("VERBOSE=true"))
           (test-buffer (get-buffer-create "*test-buffer*"))
           (client (agent-shell-anthropic-make-claude-client :buffer test-buffer)))
      (unwind-protect
          (progn
            ;; Verify environment variables include empty API key for login
            (should (member "ANTHROPIC_API_KEY=" (map-elt client :environment-variables)))
            (should (member "VERBOSE=true" (map-elt client :environment-variables))))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))

    ;; Test with function-based API key
    (let* ((agent-shell-anthropic-authentication `(:api-key ,(lambda () "dynamic-key")))
           (agent-shell-anthropic-claude-acp-command '("claude-agent-acp"))
           (agent-shell-anthropic-claude-environment '())
           (test-buffer (get-buffer-create "*test-buffer*"))
           (client (agent-shell-anthropic-make-claude-client :buffer test-buffer))
           (env-vars (map-elt client :environment-variables)))
      (unwind-protect
          (should (member "ANTHROPIC_API_KEY=dynamic-key" env-vars))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))

    ;; Test error on invalid authentication
    (let* ((agent-shell-anthropic-authentication '())
           (agent-shell-anthropic-claude-acp-command '("claude-agent-acp"))
           (test-buffer (get-buffer-create "*test-buffer*")))
      (unwind-protect
          (should-error (agent-shell-anthropic-make-claude-client :buffer test-buffer)
                        :type 'error)
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))

    ;; Test with agent-shell-make-environment-variables and :inherit-env t
    (let* ((agent-shell-anthropic-authentication '(:api-key "test-key"))
           (agent-shell-anthropic-claude-acp-command '("claude-agent-acp"))
           (process-environment '("EXISTING_VAR=existing_value"))
           (agent-shell-anthropic-claude-environment (agent-shell-make-environment-variables
                                                      "NEW_VAR" "new_value"
                                                      :inherit-env t))
           (test-buffer (get-buffer-create "*test-buffer*"))
           (client (agent-shell-anthropic-make-claude-client :buffer test-buffer))
           (env-vars (map-elt client :environment-variables)))
      (unwind-protect
          (progn
            (should (member "ANTHROPIC_API_KEY=test-key" env-vars))
            (should (member "NEW_VAR=new_value" env-vars))
            (should (member "EXISTING_VAR=existing_value" env-vars)))
        (when (buffer-live-p test-buffer)
          (kill-buffer test-buffer))))))

(ert-deftest agent-shell-anthropic-default-model-id-function-test ()
  "Test that agent-shell-anthropic-default-model-id accepts a function."
  (let* ((config (agent-shell-anthropic-make-claude-code-config))
         (default-model-id-fn (map-elt config :default-model-id)))

    ;; Test with nil value
    (let ((agent-shell-anthropic-default-model-id nil))
      (should (null (funcall default-model-id-fn))))

    ;; Test with string value
    (let ((agent-shell-anthropic-default-model-id "claude-opus-4-6"))
      (should (string= (funcall default-model-id-fn) "claude-opus-4-6")))

    ;; Test with function value
    (let ((agent-shell-anthropic-default-model-id (lambda () "dynamic-model-id")))
      (should (string= (funcall default-model-id-fn) "dynamic-model-id")))

    ;; Test with function that returns nil
    (let ((agent-shell-anthropic-default-model-id (lambda () nil)))
      (should (null (funcall default-model-id-fn))))))

(provide 'agent-shell-anthropic-tests)
;;; agent-shell-anthropic-tests.el ends here
