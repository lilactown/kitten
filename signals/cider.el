(require 'reflex)
(require 'bind-key)

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
 )


(reflex/provide-signals
 cider-repl-mode-map
 (:repl/jack-in sesman-start)
 (:repl/quit sesman-quit)
 (:repl/interrupt cider-interrupt))
