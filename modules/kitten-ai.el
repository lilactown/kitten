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

(setq openai-key #'openai-key-auth-source)
;; (setq openai--show-log nil)

(reflex/provide-signals
 copilot-mode
 (:complete/accept copilot-accept-completion))


(reflex/provide-signals
 global
 (:ai/message spiel-message)
 (:ai/message-region spiel-message-insert-fenced-region)
 (:ai/session spiel-show-session)
 (:ai/prompt spiel-edit-prompt))

(provide 'kitten-ai)

;;; kitten-ai.el ends here.
