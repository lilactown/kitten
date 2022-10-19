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

;; smart parenthesis management
(use-package smartparens
  :init (require 'smartparens-config)
  :config
  (smartparens-global-mode t) ;; These options can be t or nil.
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t))

;; meow paren state
(require 'meow)

(setq meow-paren-keymap (make-keymap))
(meow-define-state paren
  "meow state for interacting with smartparens"
  :lighter " [P]"
  :keymap meow-paren-keymap)

;; meow-define-state creates the variable
(setq meow-cursor-type-paren 'hollow)

(meow-define-keys 'paren
  '("<escape>" . meow-normal-mode)
  '("i" . sp-forward-sexp)
  '("m" . sp-backward-sexp)
  '("n" . sp-down-sexp)
  '("e" . sp-up-sexp)
  '("s" . sp-forward-slurp-sexp)
  '("t" . sp-forward-barf-sexp)
  '("S" . sp-backward-barf-sexp)
  '("T" . sp-backward-slurp-sexp)
  '("r" . sp-raise-sexp)
  '("u" . meow-undo))

(require 'reflex)

(reflex/provide-signal :state/lisp meow-paren-mode)

;; Color hex color codes so you can see the actual color.
(use-package rainbow-mode)

(provide 'kitten-lisp)
