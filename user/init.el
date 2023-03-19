;;; init.el --- File run on startup, configuring your emacs

;;; Commentary:

;;; Code:

(require 'kitten-emacs)
(require 'kitten-ui)

;; sync org files across devices
(setq kitten-org-dir "~/iCloud/org/personal")
(setq kitten-org-agenda-files
      (list "inbox.org" "agenda.org" "notes.org" "projects.org" "events.org"
            "~/iCloud/org/work/projects.org"))

(require 'kitten-org)

(require 'kitten-capture)
(require 'kitten-meow)
(require 'kitten-project)
(require 'kitten-completion)

;; Programming languages
(use-package markdown-mode)

(require 'kitten-lsp)
(require 'kitten-git)
(require 'kitten-lisp)
(require 'kitten-clojure)
(require 'kitten-ocaml)

;; start server to allow emacsclient usage
(require 'server)
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(unless (server-running-p)
  (server-start))

;; Emacs with no decorations
(setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

;; Load keybindings
(load (expand-file-name "keybinds.el" kitten-user-dir))

;;; init.el ends here
