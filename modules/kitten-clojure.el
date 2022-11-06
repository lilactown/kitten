;;; kitten-clojure.el --- Clojure config for kitten -*- lexical-binding: t -*-
;;; Commentary:
;; Package-Requires: ((use-package) (cider) (flycheck) (lsp-mode))
;;; Code:

(require 'reflex)

(require 'bind-key)

(use-package flycheck
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.ednl$" . clojure-mode))
  :config
  (setq clojure-toplevel-inside-comment-form t)
  (bind-keys
   :map clojure-mode-map
   ;; disable vertical aligning of forms on a shortcut i accidentally press a lot
   ("C-c SPC" . nil)))

(use-package cider
  :config
  (require 'flycheck-clj-kondo))

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
 (:eval/pprint-last-sexp cider-pprint-eval-last-sexp)
 (:eval/replace-last-sexp cider-eval-last-sexp-and-replace)

 (:repl/jack-in sesman-start)
 (:repl/connect kitten-clojure/connect)
 (:repl/quit sesman-quit)
 (:repl/switch-to cider-switch-to-repl-buffer)

 (:help/doc cider-doc)
 (:help/apropos cider-apropos))


(reflex/provide-signals
 cider-repl-mode-map
 (:repl/clear cider-repl-clear-buffer)
 (:repl/interrupt cider-interrupt)
 (:repl/jack-in sesman-start)
 (:repl/quit sesman-quit))


(defvar kitten-clojure/major (make-sparse-keymap))
(define-prefix-command 'kitten-clojure/major)

(bind-keys
 :map kitten-clojure/major
 ("g g" . cider-find-var)
 ("h h" . cider-doc)
 ("h a" . cider-apropos))

(reflex/provide-signal :mode/major kitten-clojure/major clojure-mode-map)


(provide 'kitten-clojure)

;;; kitten-clojure.el ends here
