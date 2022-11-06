
;;; init.el --- Initialize Emacs

;;; Commentary: N/A

;;; Code:

;; convenience function for loading files in this config repo
(defun load! (file)
  "Load a file FILE from \"user-emacs-directory\"."
  (load-file (expand-file-name file user-emacs-directory)))


;; `bootstrap.el' contains boilerplate code related to package management. You
;; can follow the same pattern if you want to split out other bits of config.
(load! "bootstrap.el")

(require 'use-package)

;; Load keybindings
(load! "keybinds.el")

;; don't use tabs ever
(setq-default indent-tabs-mode nil)

(require 'kitten-ui)
(require 'kitten-org)
(require 'kitten-meow)

;; Programming languages
(use-package markdown-mode)

(require 'kitten-git)
(require 'kitten-lisp)
(require 'kitten-clojure)
(require 'kitten-project)
;; Not a fan of trailing whitespace in source files, strip it out when saving.
(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

;; Don't make backup~ files
(setq make-backup-files nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval define-clojure-indent
           (:require 0)
           (:import
            '(0
              (0)))
           (defrecord
             '(1 nil
                 (:defn)))
           (forv 1)
           (for+ 1)
           (future-with 1)
           (start-unless 1)
           (stop-when 1)
           (do-at 1)
           (thrown\? 1)
           (thrown-with-msg\? 2)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; init.el ends here
