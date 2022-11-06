;;; kitten-completion.el --- Package configuration for completions & associated UI

;;; Commentary:

;;; Code:


(require 'use-package)
(require 'reflex)

;; Replacements for most completing-read functions
(use-package consult)

(reflex/provide-signals
 global
 (:buffer/switch consult-buffer)
 (:buffer/project-switch consult-project-buffer)
 (:emacs/apropos consult-apropos)
 (:search/file consult-find)
 (:search/grep consult-ripgrep)
 (:search/line consult-line))

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

;; Embark & consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'kitten-completion)

;;; kitten-completion.el ends here.
