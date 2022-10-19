;; convenience function for loading files in this config repo
(defun load! (file)
  (load-file (expand-file-name file user-emacs-directory)))

(push "~/.config/emacs/lisp" load-path)

;; `bootstrap.el' contains boilerplate code related to package management. You
;; can follow the same pattern if you want to split out other bits of config.
(load! "bootstrap.el")

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

(use-package flycheck
  :init (global-flycheck-mode))

;; Language-specific packages
(use-package org)
(use-package markdown-mode)
(use-package flycheck-clj-kondo)
(use-package cider
  :config
  (require 'flycheck-clj-kondo)
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

(set-frame-font "Cascadia Mono PL-13")

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

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode 1))

;; Replacements for most completing-read functions
(use-package consult)

;; Provides autocomplete minibuffer for completing-read
(use-package vertico
  :straight '(vertico :files (:defaults "extensions/*")
                      :includes (vertico-buffer
                                 vertico-directory
                                 vertico-flat
                                 vertico-indexed
                                 vertico-mouse
                                 vertico-quick
                                 vertico-repeat
                                 vertico-reverse))
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

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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
