;;; keybinds.el --- Custom keybindings

;;; Commentary:

;; Global keybindings

;;; Code:

(require 'use-package)

(require 'bind-key)
(require 'reflex)

;; global commands
(bind-keys*
 ("C-c SPC" . execute-extended-command))


(eval-after-load 'embark
  '(bind-key* "C-c ." 'embark-act))

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
 ("d" . kill-buffer))

(reflex/bind-signals
 kitten/buffer
 ("b" :buffer/switch)
 ("B" :buffer/project-switch))

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
 :prefix "C-c s")

(reflex/bind-signals
 kitten/search
 ("a" :emacs/apropos)
 ("f" :search/file)
 ("g" :search/grep)
 ("l" :search/line))

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

;; REPL
(reflex/bind-signals
 global
 ("C-c r '" :repl/jack-in)
 ("C-c r c" :repl/connect)
 ("C-c r l" :repl/clear)
 ("C-c r q" :repl/quit)
 ("C-c r s" :repl/switch-to)
 ("C-c r g" :repl/goto-defun)
 ("C-c r h h" :repl/doc-view)
 ("C-c r h a" :repl/doc-apropos))

;; Help in IDE
;; (reflex/bind-signals
;;  global
;;  ("C-c m h h" :help/doc)
;;  ("C-c m h a" :help/apropos))

;; Org mode
(reflex/bind-signals
 global
 ("C-c n a" :notes/agenda)
 ("C-c n c" :notes/capture)
 ("C-c n i" :notes/inbox)
 ("C-c n g" :notes/capture-external)
 ("C-c n k f" :roam/find-node)
 ("C-c n k c" :roam/capture))

(reflex/bind-signals
 org-mode-map
 ("C-c m RET" :notes/return)
 ("C-c m c i" :notes/clock-in)
 ("C-c m c o" :notes/clock-out)
 ("C-c m c a" :notes/insert-time-stamp)
 ("C-c m c t" :notes/insert-time-stamp-inactive)
 ("C-c m r" :notes/refile)
 ("C-c m u c" :notes/update-cookies)
 ("C-c m u d" :notes/update-deadline)
 ("C-c m u e" :notes/update-effort)
 ("C-c m u s" :notes/update-schedule)
 ("C-c m u t" :notes/update-tags)
 ;; roam
 ("C-c m k i" :roam/insert-node)
 ("C-c m k l" :roam/toggle)
 ("C-c m k o" :roam/create-id)
 ("C-c m k x" :roam/extract))

;; Basic elisp
(reflex/provide-signals
 emacs-lisp-mode-map
 ;; Eval
 (:eval/buffer eval-buffer)
 (:eval/defun eval-defun)
 (:eval/last-sexp eval-last-sexp)
 (:eval/macroexpand-last-sexp pp-macroexpand-last-sexp)
 (:eval/pprint-last-sexp pp-eval-last-sexp)

 ;; REPL
 (:repl/goto-defun find-function)
 (:repl/doc-apropos consult-apropos))

;;; keybinds.el ends here
