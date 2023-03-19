;;; kitten-lsp.el --- Configuration for an IDE using LSP
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'reflex)

(use-package eglot)

(reflex/provide-signals
 global
 (:lsp/start eglot)
 (:lsp/stop eglot-shutdown)
 (:lsp/actions eglot-code-actions)
 (:lsp/find-refs xref-find-references)
 (:lsp/find-defs xref-find-definitions-other-window)
 (:lsp/buffer-diagnostics consult-flymake)
 (:lsp/project-diagnostics flymake-show-project-diagnostics))

(use-package tree-sitter
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)


(provide 'kitten-lsp)

;;; kitten-lsp.el ends here.
