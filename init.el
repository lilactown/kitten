;; convenience function for loading files in this config repo
(defun load! (file)
  (load-file (expand-file-name file user-emacs-directory)))

;; `bootstrap.el' contains boilerplate code related to package management. You
;; can follow the same pattern if you want to split out other bits of config.
(load! "bootstrap.el")

(load! "reflex.el")

;; Load keybindings
(load! "keybinds.el")

;; don't use tabs ever
(setq-default indent-tabs-mode nil)

;; modal editing
(use-package meow
  :config
  (load! "meow.el")
  (meow-setup-indicator)
  (meow-global-mode 1))

;; Powerful Git integration.
(use-package magit
  :commands magit-status)

;; Language-specific packages
(use-package org)
(use-package markdown-mode)
(use-package cider
  :config
  (load! "signals/cider.el"))

;; Color hex color codes so you can see the actual color.
(use-package rainbow-mode)

;; Programming languages
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
;; Disable in minibuffer https://emacs.stackexchange.com/a/29342
(defun init--inhibit-electric-pair-mode (char)
  (minibufferp))
(setq electric-pair-inhibit-predicate #'init--inhibit-electric-pair-mode)
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

;; Replacements for most completing-read functions
(use-package consult)

;; Provides autocomplete minibuffer for completing-read
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; You can think of embark-act as a keyboard-based version of a right-click contextual menu.
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

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
