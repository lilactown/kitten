;;; init.el --- File run on startup, configuring your emacs

;;; Commentary:

;;; Code:

(require 'kitten-git)
(require 'kitten-emacs)
(require 'kitten-ui)
(require 'kitten-navigation)

;; sync org files across devices
(setq kitten-org-dir "~/iCloud/org/personal")
(setq org-agenda-files
      (append
       (list "roam/inbox.org"
             "roam/agenda.org"
             "~/org/gcal/events.org"
             "~/iCloud/org/personal/projects.org"
             (concat kitten-org-dir "/roam/areas")
             (concat kitten-org-dir "/roam/teams")
             (concat kitten-org-dir "/roam/projects"))))

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

;; start server to allow emacsclient usage
(require 'server)
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(unless (server-running-p)
  (server-start))

;; Emacs with no decorations
;; (setq default-frame-alist '((undecorated . t)))
(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

;; Load keybindings
(load (expand-file-name "keybinds.el" kitten-user-dir))

(load-theme 'catppuccin t)

;;; init.el ends here
