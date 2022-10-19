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
  '("<backspace>" . sp-backward-unwrap-sexp)

  '("l" . sp-end-of-sexp)
  '("j" . sp-beginning-of-sexp)
  '("u" . meow-undo)
  '("y" . meow-save)
  '("m" . sp-backward-sexp)
  '("n" . sp-down-sexp)
  '("e" . sp-up-sexp)
  '("i" . sp-forward-sexp)
  '("k" . sp-kill-sexp)

  '("w" . sp-select-next-thing)
  '("b" . sp-select-previous-thing)
  '("p" . meow-yank)
  '("a" . meow-append)
  '("r" . sp-raise-sexp)
  '("s" . sp-forward-slurp-sexp)
  '("t" . sp-forward-barf-sexp)
  '("S" . sp-backward-barf-sexp)
  '("T" . sp-backward-slurp-sexp)
  '("g" . meow-cancel-selection)

  '("[" . sp-wrap-square)
  '("{" . sp-wrap-curly)
  '("(" . sp-wrap-round))

;; Color hex color codes so you can see the actual color.
(use-package rainbow-mode)

(provide 'kitten-lisp)
