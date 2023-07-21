;;; kitten-ai --- Configuration for AI tooling
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'reflex)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

(use-package org-ai
  :straight (:type git :host github :repo "lilactown/org-ai" :local-repo "org-ai"
                   :files ("*.el" "README.md" "snippets")))

(reflex/provide-signals
 copilot-mode
 (:complete/accept copilot-accept-completion))

(provide 'kitten-ai)

;;; kitten-ai.el ends here.
