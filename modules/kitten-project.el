;;; kitten-project --- Basic projectile config
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package projectile
  :init (projectile-mode +1))

(use-package projectile-ripgrep)

(provide 'kitten-project)

;;; kitten-project.el ends here.
