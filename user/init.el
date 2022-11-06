;;; init.el --- File run on startup, configuring your emacs

;;; Commentary:

;;; Code:

(require 'kitten-emacs)
(require 'kitten-ui)

;; set font
(set-frame-font "Cascadia Mono PL-13")

(require 'kitten-org)
(require 'kitten-meow)

;; Programming languages
(use-package markdown-mode)

(require 'kitten-git)
(require 'kitten-lisp)
(require 'kitten-clojure)
(require 'kitten-project)
(require 'kitten-completion)

;; Load keybindings
(load "~/.config/kitten/keybinds.el")

(setq custom-file (expand-file-name "custom.el" kitten-user-dir))
(load custom-file)

;;; init.el ends here
