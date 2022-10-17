;; `bootstrap.el' contains boilerplate code related to package management. You
;; can follow the same pattern if you want to split out other bits of config.
(load-file (expand-file-name "bootstrap.el" user-emacs-directory))
(load-file (expand-file-name "keybinds.el" user-emacs-directory))
(load-file (expand-file-name "meow.el" user-emacs-directory))

;; modal editing
(use-package meow
  :config
  (meow-setup)
  (meow-setup-indicator)
  (meow-global-mode 1))

;; Powerful Git integration.
(use-package magit)

;; Language-specific packages
(use-package org)
(use-package markdown-mode)
;(use-package cider)

;; Color hex color codes so you can see the actual color.
(use-package rainbow-mode)

(use-package cider)
(use-package rainbow-delimiters
  :hook ((cider-repl-mode
          clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode
          inferior-emacs-lisp-mode)
         . rainbow-delimiters-mode))

;; surrounding selections with parens
(electric-pair-mode)
(setq electric-pair-inhibit-predicate 'ignore)
(setq electric-pair-skip-self t)

;; Remove scroll bars when not needed and borders around them
(fringe-mode 0)

(global-display-line-numbers-mode)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (setq doom-dracula-brighter-comments t)
  (setq doom-dracula-comment-bg nil)
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package embark
;  :bind
;  (("C-." . embark-act)         ;; pick some comfortable binding
;   ;("C-;" . embark-dwim)        ;; good alternative: M-.
;   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; Not a fan of trailing whitespace in source files, strip it out when saving.
(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

;; Don't make backup~ files
(setq make-backup-files nil)

;; Enabling desktop-save-mode will save and restore all buffers between sessions
(setq desktop-restore-frames nil)
(desktop-save-mode 1)

;; start in full screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; hide toolbar
(tool-bar-mode -1)

;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so window
;; borders will match the enabled theme.
(and (or (daemonp)
         (display-graphic-p))
     (require 'ns-auto-titlebar nil t)
     (ns-auto-titlebar-mode +1))
