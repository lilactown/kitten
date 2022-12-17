;;; kitten-ocaml --- Configuration for OCaml development
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package tuareg)

(use-package dune)

(require 'reflex)

(reflex/provide-signals
 tuareg-mode-map
 (:repl/jack-in run-ocaml)
 (:repl/quit tuareg-kill-ocaml)

 (:eval/buffer tuareg-eval-buffer)
 (:eval/last-sexp tuareg-eval-phrase)
 (:eval/region tuareg-eval-region))

(provide 'kitten-ocaml)

;;; kitten-ocaml.lsp ends here.
