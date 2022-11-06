;;; init.el --- Initialize Emacs

;;; Commentary: N/A

;;; Code:

;; `bootstrap.el' contains boilerplate code related to package management. You
;; can follow the same pattern if you want to split out other bits of config.
(load (expand-file-name "bootstrap.el" user-emacs-directory))

;; add lisp & modules to load-path
(push (expand-file-name "lisp" user-emacs-directory) load-path)
(push (expand-file-name "modules" user-emacs-directory) load-path)

;; custom var for kitten emacs files
(defvar kitten-user-dir "~/.config/kitten")

(setq custom-file (expand-file-name "init.el" kitten-user-dir))

(load (expand-file-name "init.el" kitten-user-dir))

;;; init.el ends here
