;;
;; global keybinds
;;

(require 'bind-key)
(require 'reflex)

;; global commands
(bind-keys
 ("C-c ;" . execute-extended-command)
 ("C-c ." . embark-act)
 ("C-c a" . consult-apropos))

;; Window managment
(bind-keys
 :prefix-map kitten/window
 :prefix "C-c w"
 ("d" . delete-window)
 ("m" . windmove-left)
 ("n" . windmove-down)
 ("e" . windmove-up)
 ("i" . windmove-right)
 ("/" . split-window-horizontally)
 ("-" . split-window-vertically))

;; Buffer management
(bind-keys
 :prefix-map kitten/buffer
 :prefix "C-c b"
 ("d" . kill-buffer)
 ("b" . consult-buffer)
 ("B" . consult-project-buffer))

;; File management
(bind-keys
 :prefix-map kitten/file
 :prefix "C-c f"
 ("f" . find-file)
 ("s" . save-buffer))

;; Magit
(bind-keys
 :prefix-map kitten/magit
 :prefix "C-c g"
 ("s" . magit-status)
 ("l" . magit-log))

;; Searching
(bind-keys
 :prefix-map kitten/search
 :prefix "C-c s"
 ("f" . consult-ripgrep)
 ("b" . consult-line))

;; Evaluating
(reflex/bind-signals
 global
 ("C-c e b" :eval/buffer)
 ("C-c e d" :eval/defun)
 ("C-c e e" :eval/last-sexp)
 ("C-c e p" :eval/pprint-expr))

(reflex/provide-signals
 emacs-lisp-mode
 (:eval/buffer eval-buffer)
 (:eval/defun eval-defun)
 (:eval/last-sexp eval-last-sexp)
 (:eval/pprint-expr pp-eval-expression))
