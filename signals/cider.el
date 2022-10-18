(require 'reflex)

(reflex/provide-signals
 clojure-mode
 (:eval/buffer cider-eval-buffer)
 (:eval/defun cider-eval-defun-at-point)
 (:eval/last-sexp cider-eval-last-sexp)
 (:eval/pprint-expr cider-eval-last-sexp))
