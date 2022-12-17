;;; kitten-emacs.el --- Configuration for general Emacs behavior
;;; Commentary:
;;; Code:

;; don't use tabs ever
(setq-default indent-tabs-mode nil)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; Not a fan of trailing whitespace in source files, strip it out when saving.
(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

;; Don't make backup~ files
(setq make-backup-files nil)

;; auto revert files when visiting the buffer
(global-auto-revert-mode)

(require 'use-package)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; You can think of embark-act as a keyboard-based version of a right-click
;; contextual menu.
(use-package embark
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; Put custom settings in custom.el
(setq custom-file (expand-file-name "custom.el" kitten-user-dir))
(load custom-file)

(provide 'kitten-emacs)

;;; kitten-emacs.el ends here.
