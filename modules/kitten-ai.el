;;; kitten-ai --- Configuration for AI tooling
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'reflex)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

;; (use-package org-ai
;;   :straight (:type git :host github :repo "lilactown/org-ai" :local-repo "org-ai"
;;                    :files ("*.el" "README.md" "snippets"))
;;   :init (add-hook 'org-mode-hook #'org-ai-mode))

(require 'spiel)
(require 'openai)

(setq openai-base-url "https://amperity-engineering.openai.azure.com/openai/deployments/gpt-35-turbo")
(setq openai-key #'openai-key-auth-source)
(setq openai-key-type :azure-api)
;; (setq openai--show-log nil)

(reflex/provide-signals
 copilot-mode
 (:complete/accept copilot-accept-completion))


(reflex/provide-signals
 global
 (:ai/prompt spiel-message)
 (:ai/prompt-with-region spiel-message-insert-fenced-region)
 (:ai/show-topic spiel-show-session))

(provide 'kitten-ai)

;;; kitten-ai.el ends here.
