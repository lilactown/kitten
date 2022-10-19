(require 'reflex)
(require 'bind-key)

(use-package flycheck-clj-kondo)
(use-package cider
  :config
  (require 'flycheck-clj-kondo))

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (clojure-mode . lsp)
         ;; if you want which-key integration
         ;;(lsp-mode . lsp-enable-which-key-integration)
         )
  :commands (lsp lsp-deferred))

(defvar kitten-clojure/connect (make-sparse-keymap))
(define-prefix-command 'kitten-clojure/connect)

(bind-keys
 :map kitten-clojure/connect
 ("c" . cider-connect-clj)
 ("C" . cider-connect-cljs))

(reflex/provide-signals
 clojure-mode-map
 (:eval/buffer cider-eval-buffer)
 (:eval/defun cider-eval-defun-at-point)
 (:eval/last-sexp cider-eval-last-sexp)
 (:eval/macroexpand-last-sexp cider-macroexpand-all)
 (:eval/pprint-last-sexp cider-eval-last-sexp)
 (:eval/replace-last-sexp cider-eval-last-sexp-and-replace)

 (:repl/jack-in sesman-start)
 (:repl/connect kitten-clojure/connect)
 (:repl/quit sesman-quit)
 (:repl/switch-to cider-switch-to-repl-buffer)

 (:help/doc cider-doc)
 (:help/apropos cider-apropos))


(reflex/provide-signals
 cider-repl-mode-map
 (:repl/jack-in sesman-start)
 (:repl/quit sesman-quit)
 (:repl/interrupt cider-interrupt))


(provide 'kitten-clojure)
