;;; init.el --- File run on startup, configuring your emacs

;;; Commentary:

;;; Code:

(require 'use-package)
(use-package sqlite3)

(require 'kitten-git)
(require 'kitten-emacs)
(require 'kitten-ui)
(require 'kitten-navigation)

;; sync org files across devices
(setq work-life-org-directory "~/iCloud/org/work")
(setq org-directory "~/Documents/org")
(setq org-agenda-files
      (list "roam/inbox.org"
            "roam/agenda.org"
            (concat org-directory "/roam/areas")
            (concat org-directory "/roam/projects")
            (concat org-directory "/roam/refs")
            (concat work-life-org-directory "/roam/areas")
            (concat work-life-org-directory "/roam/teams")
            (concat work-life-org-directory "/roam/projects")
            (concat work-life-org-directory "/roam/refs")))

(require 'kitten-org)

(require 'kitten-capture)
(require 'kitten-meow)
(require 'kitten-completion)

;; Programming languages
(use-package markdown-mode)

(require 'kitten-lsp)
(require 'kitten-lisp)
(require 'kitten-clojure)
(require 'kitten-ocaml)
(require 'kitten-ai)


;; start server to allow emacsclient usage
(require 'server)
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(unless (server-running-p)
  (server-start))

;; Emacs with no decorations
;; (setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))
;; start in full screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Load keybindings
(load (expand-file-name "keybinds.el" kitten-user-dir))

(load-theme 'catppuccin t)

;;; init.el ends here
