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

;; Color hex color codes so you can see the actual color.
(use-package rainbow-mode)

;; surrounding selections with parens
(electric-pair-mode)

;; Disable in minibuffer https://emacs.stackexchange.com/a/29342
(defun init--inhibit-electric-pair-mode (char)
  (minibufferp))
(setq electric-pair-inhibit-predicate #'init--inhibit-electric-pair-mode)
(setq electric-pair-skip-self t)

(provide 'kitten-lisp)
