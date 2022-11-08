;;; init.el --- File run on startup, configuring your emacs

;;; Commentary:

;;; Code:

(require 'kitten-emacs)
(require 'kitten-ui)
(require 'kitten-org)
(require 'kitten-meow)
(require 'kitten-project)
(require 'kitten-completion)

;; Programming languages
(use-package markdown-mode)

(require 'kitten-git)
(require 'kitten-lisp)
(require 'kitten-clojure)

;; start server to allow emacsclient usage
(require 'server)
(unless (server-running-p)
  (server-start))

;; Emacs with no decorations
(setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

;; Load keybindings
(load (expand-file-name "keybinds.el" kitten-user-dir))

(setq custom-file (expand-file-name "custom.el" kitten-user-dir))
(load custom-file)

;;; init.el ends here
