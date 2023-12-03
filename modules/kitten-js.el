;;; kitten-js --- Configuration for JS & TS development
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'js)
(require 'reflex)

;; (use-package typescript-mode
;;   :after tree-sitter
;;   :config
;;   ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
;;   ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
;;   (define-derived-mode typescriptreact-mode typescript-mode
;;     "TypeScript TSX")

;;   ;; use our derived mode for tsx files
;;   (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
;;   ;; by default, typescript-mode is mapped to the treesitter typescript parser
;;   ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
;; (use-package tsi
;;   :after tree-sitter
;;   :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;;   (add-hook 'js-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;   (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (treesit-language-available-p 'javascript)

(use-package typescript-ts-mode
  :init
  ;; Associate ts files with `typescript-ts-mode'.
  (add-to-list 'auto-mode-alist (cons "\\.cts\\'" 'typescript-ts-mode))
  (add-to-list 'auto-mode-alist (cons "\\.ts\\'" 'typescript-ts-mode))

  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  :custom (typescript-ts-mode-indent-offset 2))

(add-to-list
 'major-mode-remap-alist
 '(javascript-mode . js-ts-mode)
 ;;(json-mode . json-ts-mode)
 )

(setq js-indent-level 2)

(use-package nodejs-repl
  :straight (:host github :repo "lilactown/nodejs-repl.el"))

(reflex/provide-signals
 js-ts-mode-map
 (:repl/jack-in nodejs-repl)
 (:repl/switch-to nodejs-repl-switch-to-repl)
 (:eval/last-sexp nodejs-repl-send-last-expression)
 (:eval/buffer nodejs-repl-send-buffer)
 (:eval/region nodejs-repl-send-region))

(reflex/provide-signals
 typescript-ts-mode-map
 (:repl/jack-in nodejs-repl)
 (:repl/switch-to nodejs-repl-switch-to-repl)
 (:eval/last-sexp nodejs-repl-send-last-expression)
 (:eval/buffer nodejs-repl-send-buffer)
 (:eval/region nodejs-repl-send-region))

(provide 'kitten-js)

;;; kitten-js.el ends here.
