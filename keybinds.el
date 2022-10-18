;;
;; global keybinds
;;

;;(require 'reflex)

(defvar keybinds--execute-command (kbd "C-c ;"))
(defvar keybinds--embark-act (kbd "C-c ."))
(defvar keybinds--apropos (kbd "C-c a"))
(defvar keybinds--buffer-prefix (kbd "C-c b"))
(defvar keybinds--file-prefix (kbd "C-c f"))
(defvar keybinds--magit-prefix (kbd "C-c g"))
(defvar keybinds--search-prefix (kbd "C-c s"))
(defvar keybinds--window-prefix (kbd "C-c w"))

(global-set-key keybinds--execute-command 'execute-extended-command)
(global-set-key keybinds--embark-act 'embark-act)
(global-set-key keybinds--apropos 'consult-apropos)

;; Window managment
(defvar keybinds--window-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'delete-window)
    (define-key map "m" 'windmove-left)
    (define-key map "n" 'windmove-down)
    (define-key map "e" 'windmove-up)
    (define-key map "i" 'windmove-right)
    (define-key map "/" 'split-window-horizontally)
    (define-key map "-" 'split-window-vertically)
    map))

(global-set-key keybinds--window-prefix keybinds--window-keymap)

;; Buffer management
(defvar keybinds--buffer-keymap
 (let ((map (make-sparse-keymap)))
    (define-key map "d" 'kill-buffer)
    (define-key map "b" 'consult-buffer)
    (define-key map "B" 'consult-project-buffer)
    map))

(global-set-key keybinds--buffer-prefix keybinds--buffer-keymap)

;; File management
(defvar keybinds--file-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'find-file)
    (define-key map "s" 'save-buffer)
    map))

(global-set-key keybinds--file-prefix keybinds--file-keymap)

;; Magit
(defvar keybinds--magit-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'magit-status)
    (define-key map "l" 'magit-log)
    map))

(global-set-key keybinds--magit-prefix keybinds--magit-keymap)

;; Searching
(defvar keybinds--search-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'consult-ripgrep)
    (define-key map "b" 'consult-line)
    map))

(global-set-key keybinds--search-prefix keybinds--search-keymap)

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
