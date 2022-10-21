;;; keybinds.el --- Global keybindings

;;; Commentary:

;; Global keybindings

;;; Code:

(require 'bind-key)
(require 'reflex)

;; global commands
(bind-keys
 ("C-c ;" . execute-extended-command)
 ("C-c ." . embark-act))

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

;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

;; Files
(bind-keys
 :prefix-map kitten/file
 :prefix "C-c f"
 ("d" . delete-file-and-buffer)
 ("f" . find-file)
 ("r" . rename-file)
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
 ("a" . consult-apropos)
 ("f" . consult-find)
 ("g" . consult-ripgrep)
 ("l" . consult-line))

;; project kitchen sink
(bind-key "C-c p" 'projectile-command-map)

;; Evaluating
(reflex/bind-signals
 global
 ("C-c e b" :eval/buffer)
 ("C-c e d" :eval/defun)
 ("C-c e e" :eval/last-sexp)
 ("C-c e m" :eval/macroexpand-last-sexp)
 ("C-c e p" :eval/pprint-last-sexp))

(reflex/provide-signals
 emacs-lisp-mode-map
 (:eval/buffer eval-buffer)
 (:eval/defun eval-defun)
 (:eval/last-sexp eval-last-sexp)
 (:eval/macroexpand-last-sexp pp-macroexpand-last-sexp)
 (:eval/pprint-last-sexp pp-eval-last-sexp))

;; REPL
(reflex/bind-signals
 global
 ("C-c r '" :repl/jack-in)
 ("C-c r c" :repl/connect)
 ("C-c r l" :repl/clear)
 ("C-c r q" :repl/quit)
 ("C-c r s" :repl/switch-to))

;; Help in IDE
(reflex/bind-signals
 global
 ("C-c m h h" :help/doc)
 ("C-c m h a" :help/apropos))

;;; keybinds.el ends here
