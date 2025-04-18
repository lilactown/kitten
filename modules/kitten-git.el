;;; kitten-git --- Basic configuration for magit
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package magit
  :straight (magit :host github :repo "magit/magit" :branch "main")
  :commands magit-status)

(provide 'kitten-git)

;;; kitten-git.el ends here.
