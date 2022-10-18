(require 'reflex)

(reflex/provide-signal :eval/buffer #'cider-eval-buffer 'clojure-mode)
(reflex/provide-signal :eval/defun #'cider-eval-defun-at-point 'clojure-mode)
(reflex/provide-signal :eval/last-sexp #'cider-eval-last-sexp 'clojure-mode)
(reflex/provide-signal :eval/pprint-expr #'cider-pprint-eval-last-sexp 'clojure-mode)
