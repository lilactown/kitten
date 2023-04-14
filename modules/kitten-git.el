;;; kitten-git --- Basic configuration for magit
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package magit
  :straight (magit :host github :repo "magit/magit" :branch "main")
  :commands magit-status)

(use-package forge
  :after magit
  :config
  (magit-add-section-hook
   'magit-status-sections-hook
   'forge-insert-requested-reviews
   'forge-insert-pullreqs
   'replace))

(use-package code-review
  :config
  (setq code-review-fill-column 80)
  (setq code-review-auth-login-marker 'code-review)
  :hook (code-review-mode-hook . #'emojify-mode))

(provide 'kitten-git)

;;; kitten-git.el ends here.
