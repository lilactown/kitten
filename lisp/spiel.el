;; spiel.el --- An interface for ChatGPT  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package openai
  :straight (:type git :host github :repo "lilactown/openai"))

(require 'markdown-mode)
(require 'spinner)
(require 'openai)
(require 'openai-chat)

;; temp setup

(defcustom spiel-conversation-dir
  (expand-file-name "spiel/conversations"
                    user-emacs-directory)
  "Var containing directory to persist conversations."
  :type 'string
  :group 'spiel)

(defcustom spiel-prompt-dir
  (expand-file-name "spiel/prompts"
                    user-emacs-directory)
  "Var containing directory to persist custom system prompts."
  :type 'string
  :group 'spiel)


(defun spiel--list-sessions ()
  ""
  (mapcar
   (lambda (filename)
     (replace-regexp-in-string (concat spiel-conversation-dir "/") "" filename))
   (spiel--get-files-in-directory spiel-conversation-dir)))

(defun spiel--session-exists-p (session)
  ""
  (let ((sessions (spiel--list-sessions)))
    (member session sessions)))

(defun spiel--list-prompts ()
  ""
  (unless (file-directory-p spiel-prompt-dir)
    (make-directory spiel-prompt-dir t))
  (mapcar
   (lambda (filename)
     (replace-regexp-in-string (concat spiel-prompt-dir "/") "" filename))
   (spiel--get-files-in-directory spiel-prompt-dir)))

(defun spiel--write-messages (session messages)
  ""
  (let ((filename (concat spiel-conversation-dir "/" session)))
    (unless (file-directory-p spiel-conversation-dir)
      (make-directory spiel-conversation-dir t))
    (with-temp-file filename
      (insert (pp messages)))))

;; (spiel--write-messages "foo" `[((role . "user") (content . "How are you?"))])

(defun spiel--read-messages (session)
  ""
  (let ((filename (concat spiel-conversation-dir "/" session)))
    (unless (file-directory-p spiel-conversation-dir)
      (make-directory spiel-conversation-dir t))
    (if (file-exists-p filename)
        (with-temp-buffer
          (insert-file-contents filename)
          (read (current-buffer)))
      [])))

(defun spiel--read-prompt (prompt)
  ""
  (let ((filename (concat spiel-prompt-dir "/" prompt)))
    (unless (file-directory-p spiel-prompt-dir)
      (make-directory spiel-conversation-dir t))
    (if (file-exists-p filename)
        (with-temp-buffer
          (insert-file-contents filename)
          (buffer-string))
      "")))

(defun spiel--write-prompt (prompt content)
  ""
  (let ((filename (concat spiel-prompt-dir "/" prompt)))
    (unless (file-directory-p spiel-prompt-dir)
      (make-directory spiel-prompt-dir t))
    (with-temp-file filename
      (insert content))))

;; (spiel--read-messages "foo")
;; (spiel--read-messages "bar")

(defun spiel--display-messages (session messages)
  ""
  (let ((buf-name (concat "*Spiel: " session "*")))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (with-output-to-temp-buffer buf-name
      (mapc (lambda (msg)
              (let-alist msg
                (princ (format "**<%s> %s:** %s\n\n"
                               (if .time (format-time-string "%D %R" .time) "?")
                               .role (string-trim .content)))
                (princ "---\n\n")))
            (reverse messages)))
    (with-current-buffer buf-name
      (markdown-view-mode))))

(defun spiel-show-session (session)
  ""
  (interactive
   (list (completing-read
                      "Session: "
                      (spiel--list-sessions))))
  (spiel--display-messages session (spiel--read-messages session)))


(defun spiel--say (session message)
  ""
  (interactive)
  (let ((messages (vconcat (spiel--read-messages session)
                           `[((role . "user")
                              (content . ,message)
                              (time . ,(current-time)))])))
    (spinner-start 'progress-bar)
    (openai-chat
     ;; remove `time` key
     (cl-map 'vector
             (lambda (msg) (assq-delete-all 'time (copy-alist msg)))
             messages)
     (lambda (data)
       (let ((choices (let-alist data .choices)))
         (let ((messages (vconcat messages
                                   (cl-map 'vector
                                           (lambda (choice)
                                             (let-alist choice
                                               (push `(time . ,(current-time))
                                                     .message)
                                               .message))
                                           choices))))
           (spinner-stop)
           (spiel--display-messages session messages)
           (spiel--write-messages
            session
            messages))))
     :parameters '(("api-version" . "2023-05-15")))))

;; (spiel--say "testing" "how are you?")
;; (spiel--say "testing" "pretend you're my grandma who loves me and answer the question.")
;; (spiel--say "testing" "Now pretend you're my son and answer the question.")


(define-minor-mode spiel-compose-mode
  "Minor mode used when composing a new message to send to a *spiel* session."
  :init-value nil
  :keymap
  `((,(kbd "C-c C-c") . spiel-message-send-and-exit)
    (,(kbd "C-c C-k") . spiel-message-kill)))

(define-minor-mode spiel-prompt-mode
  "Minor mode used when composing a new prompt."
  :init-value nil
  :keymap
  `((,(kbd "C-c C-c") . spiel-prompt-save-and-exit)
    (,(kbd "C-c C-k") . spiel-message-kill)))

(defun spiel-message-kill ()
  ""
  (interactive)
  (kill-buffer (current-buffer)))

(defun spiel-message-send-and-exit ()
  ""
  (interactive)
  (goto-char (point-min))
  (forward-char 2)
  (let ((session (buffer-substring-no-properties (point)
                                                 (line-end-position)))
        (msg (progn
               (forward-line)
               (buffer-substring-no-properties (line-beginning-position) (point-max)))))
    (kill-buffer)
    (spiel--say session msg)))

(defun spiel--message-buffer-name (session)
  ""
  (concat "*Spiel message: " session "*"))

(defun spiel--get-files-in-directory (directory)
  "Return a list of files in DIRECTORY."
  (let ((files '()))
    (dolist (file (directory-files directory t nil nil))
      (when (file-regular-p file)
        (setq files (cons file files))))
    files))

(defun spiel-edit-prompt (prompt)
  ""
  (interactive (list (completing-read
                      "Prompt: "
                      (spiel--list-prompts))))
  (switch-to-buffer (concat "*Spiel prompt: " prompt "*"))
  (insert "# " prompt "\n\n")
  (markdown-mode)
  (spiel-prompt-mode 1))

(defun spiel-prompt-save-and-exit ()
  ""
  (interactive)
  (goto-char (point-min))
  (forward-char 2)
  (let ((prompt (buffer-substring-no-properties (point)
                                                (line-end-position)))
        (content (progn
                   (forward-line)
                   (buffer-substring-no-properties (line-beginning-position)
                                                   (point-max)))))
    (spiel--write-prompt prompt content)
    (kill-buffer)))

(defun spiel-message (session)
  ""
  (interactive (list (completing-read
                      "Session: "
                      (spiel--list-sessions))))
  (let ((open-message (lambda ()
                        (switch-to-buffer (spiel--message-buffer-name session))
                        (insert "# " session "\n\n")
                        (with-silent-modifications
                          (put-text-property 1 (- (point) 1) 'read-only t))
                        (markdown-mode)
                        (spiel-compose-mode 1))))
    (if (spiel--session-exists-p session)
        (funcall open-message)
      (let ((prompt (completing-read
                     "Prompt (empty is fine): "
                     (spiel--list-prompts))))
        (message "prompt: %s" prompt)
        (if (string-blank-p prompt)
            (funcall open-message)
          (spiel--write-messages
           session
           `[((role . "system")
              (content . ,(spiel--read-prompt prompt))
              (time . ,(current-time)))])
          (funcall open-message))))))

(defun spiel-message-insert-region (start end)
  ""
  (interactive "r")
  (let ((session (completing-read "Session: " (spiel--list-sessions)))
        (region-text (buffer-substring-no-properties start end)))
   (let ((msg-buffer (get-buffer (spiel--message-buffer-name session))))
     (if msg-buffer
         (switch-to-buffer msg-buffer)
       (spiel-message session))
     (goto-char (point-max))
     (insert "\n" region-text))))

(defun spiel-message-insert-fenced-region (start end)
  ""
  (interactive "r")
  (let ((session (completing-read "session: " (spiel--list-sessions)))
        (region-text (buffer-substring-no-properties start end)))
   (let ((msg-buffer (get-buffer (spiel--message-buffer-name session))))
     (if msg-buffer
         (switch-to-buffer msg-buffer)
       (spiel-message session))
     (goto-char (point-max))
     (insert "```" "\n" region-text "\n" "```"))))


;; (spiel-message "testing")


;;
;; dev
;;

;; (spiel--say "elisp" "How do I decrement a number in elisp?")

;; (spiel--say "elisp" "In Emacs elisp, how do I reverse a list?")

;; (spiel--say "elisp" "In Emacs elisp, how do I get the buffer contents starting from a certain line to the end?")


;; (spiel--say "elisp" "Your last answer was wrong. `point-at-bol` scans forward N lines from the current location the point is in the buffer. How can I get the point at the beginning of line 2?")

;; (spiel--say "elisp" "Your last answer was wrong. `line-beginning-position` scans forward N lines from the current location the point is in the buffer. How can I get the point at the beginning of line 2?")


(provide 'spiel)

;;; spiel.el ends here
