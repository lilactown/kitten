;; convenience function for loading files in this config repo
(defun load! (file)
  (load-file (expand-file-name file user-emacs-directory)))

(push "~/.config/emacs/lisp" load-path)
(push "~/.config/emacs/modules" load-path)

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

(require 'kitten-ui)

;; Programming languages
(use-package org)
(use-package markdown-mode)

(require 'kitten-git)
(require 'kitten-lisp)
(require 'kitten-clojure)
(require 'kitten-project)

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
