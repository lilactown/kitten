;;
;; global keybinds
;;

(defvar keybinds--execute-command (kbd "C-c ;"))
(defvar keybinds--embark-act (kbd "C-c ."))
;(defvar keybinds--embark-prefix (kbd "C-c a"))
(defvar keybinds--buffer-prefix (kbd "C-c b"))
(defvar keybinds--file-prefix (kbd "C-c f"))
(defvar keybinds--magit-prefix (kbd "C-c g"))
(defvar keybinds--window-prefix (kbd "C-c w"))

(global-set-key keybinds--execute-command 'execute-extended-command)
(global-set-key keybinds--embark-act 'embark-act)

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
    (define-key map "b" 'switch-to-buffer)
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
