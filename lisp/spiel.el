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

(defun spiel--list-session ()
  ""
  (mapcar
   (lambda (filename)
     (replace-regexp-in-string (concat spiel-conversation-dir "/") "" filename))
   (spiel--get-files-in-directory spiel-conversation-dir)))

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
                      (spiel--list-session))))
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
  :init-value t
  :keymap
  `((,(kbd "C-c C-c") . spiel-prompt-send-and-exit)
    (,(kbd "C-c C-k") . spiel-prompt-kill)))

;; (defun spiel--prompt-buffer-topic ()
;;   "https://stackoverflow.com/a/66592073"
;;   (nth
;;    1
;;    (assoc
;;     "TITLE"
;;     (org-element-map (org-element-parse-buffer 'greater-element) '(keyword)
;;       (lambda (kwd)
;;         (let ((data (cadr kwd)))
;;           (list (plist-get data :key)
;;                 (plist-get data :value))))))))

(defun spiel-prompt-kill ()
  ""
  (interactive)
  (kill-buffer (current-buffer)))

(defun spiel-prompt-send-and-exit ()
  ""
  (interactive)
  (goto-char (point-min))
  (forward-char 2)
  (let ((session (buffer-substring-no-properties (point)
                                               (line-end-position)))
        (prompt (progn
                  (forward-line)
                  (buffer-substring-no-properties (line-beginning-position) (point-max)))))
    ;; (message "%s" prompt)
    (kill-buffer)
    (spiel--say session prompt)))

(defun spiel--prompt-buffer-name (session)
  ""
  (concat "*Spiel prompt: " session "*"))

(defun spiel--get-files-in-directory (directory)
  "Return a list of files in DIRECTORY."
  (let ((files '()))
    (dolist (file (directory-files directory t nil nil))
      (when (file-regular-p file)
        (setq files (cons file files))))
    files))

(defun spiel-prompt (session)
  ""
  (interactive (list (completing-read
                      "Session: "
                      (spiel--list-session))))
  (switch-to-buffer (spiel--prompt-buffer-name session))
  (insert "# " session "\n\n")
  (with-silent-modifications
    (put-text-property 1 (- (point) 1) 'read-only t))
  (markdown-mode)
  (spiel-compose-mode 1))

(defun spiel-prompt-with-region (start end)
  ""
  (interactive "r")
  (let ((session (completing-read "Session: " (spiel--list-session)))
        (region-text (buffer-substring-no-properties start end)))
   (let ((prompt-buffer (get-buffer (spiel--prompt-buffer-name session))))
      (unless prompt-buffer
        (spiel-prompt session))
      (switch-to-buffer (get-buffer (spiel--prompt-buffer-name session)))
      (goto-char (point-max))
      (insert "\n" region-text))))

(defun spiel-prompt-with-fenced-region (start end)
  ""
  (interactive "r")
  (let ((session (completing-read "session: " (spiel--list-session)))
        (region-text (buffer-substring-no-properties start end)))
   (let ((prompt-buffer (get-buffer (spiel--prompt-buffer-name session))))
      (unless prompt-buffer
        (spiel-prompt session))
      (switch-to-buffer (get-buffer (spiel--prompt-buffer-name session))
      (goto-char (point-max))
      (insert "```" "\n" region-text "\n" "```")))))


;; (spiel-prompt "testing")


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
