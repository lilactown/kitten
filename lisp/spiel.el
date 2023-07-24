;; spiel.el --- An interface for ChatGPT  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package openai
  :straight (:type git :host github :repo "lilactown/openai"))

(require 'markdown-mode)
(require 'openai)
(require 'openai-chat)

;; temp setup

(setq openai-base-url "https://amperity-engineering.openai.azure.com/openai/deployments/gpt-35-turbo")
(setq openai-key #'openai-key-auth-source)
(setq openai-key-type :azure-api)
;; (setq openai--show-log nil)


(defcustom spiel-conversation-dir
  (expand-file-name "spiel/conversations"
                    user-emacs-directory)
  "Var containing directory to persist conversations."
  :type 'string
  :group 'spiel)

(defun spiel--list-topics ()
  ""
  (mapcar
   (lambda (filename)
     (replace-regexp-in-string (concat spiel-conversation-dir "/") "" filename))
   (spiel--get-files-in-directory spiel-conversation-dir)))

(defun spiel--write-messages (topic messages)
  ""
  (let ((filename (concat spiel-conversation-dir "/" topic)))
    (unless (file-directory-p spiel-conversation-dir)
      (make-directory spiel-conversation-dir t))
    (with-temp-file filename
      (insert (pp messages)))))

;; (spiel--write-messages "foo" `[((role . "user") (content . "How are you?"))])

(defun spiel--read-messages (topic)
  ""
  (let ((filename (concat spiel-conversation-dir "/" topic)))
    (unless (file-directory-p spiel-conversation-dir)
      (make-directory spiel-conversation-dir t))
    (if (file-exists-p filename)
        (with-temp-buffer
          (insert-file-contents filename)
          (read (current-buffer)))
      [])))

;; (spiel--read-messages "foo")
;; (spiel--read-messages "bar")

(defun spiel--display-messages (topic messages)
  ""
  (let ((buf-name (concat "*Spiel: " topic "*")))
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

(defun spiel-show-topic (topic)
  ""
  (interactive
   (list (completing-read
                      "Topic: "
                      (spiel--list-topics))))
  (spiel--display-messages topic (spiel--read-messages topic)))

(defun spiel--say (topic message)
  ""
  (interactive)
  (let ((messages (vconcat (spiel--read-messages topic)
                           `[((role . "user")
                              (content . ,message)
                              (time . ,(current-time)))])))
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
           (spiel--display-messages topic messages)
           (spiel--write-messages
            topic
            messages))))
     :parameters '(("api-version" . "2023-05-15")))))



;; (spiel--say "testing" "how are you?")
;; (spiel--say "testing" "pretend you're my grandma who loves me and answer the question.")
;; (spiel--say "testing" "Now pretend you're my son and answer the question.")



(define-minor-mode spiel-compose-mode
  "Minor mode used when composing a new message to send to a *spiel* topic."
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
  (let ((topic (buffer-substring-no-properties (point)
                                               (line-end-position)))
        (prompt (progn
                  (forward-line)
                  (buffer-substring-no-properties (line-beginning-position) (point-max)))))
    ;; (message "%s" prompt)
    (kill-buffer)
    (spiel--say topic prompt)))

(defun spiel--prompt-buffer-name (topic)
  ""
  (concat "*Spiel prompt: " topic "*"))

(defun spiel--get-files-in-directory (directory)
  "Return a list of files in DIRECTORY."
  (let ((files '()))
    (dolist (file (directory-files directory t nil nil))
      (when (file-regular-p file)
        (setq files (cons file files))))
    files))

(defun spiel-prompt (topic)
  ""
  (interactive (list (completing-read
                      "Topic: "
                      (spiel--list-topics))))
  (switch-to-buffer (spiel--prompt-buffer-name topic))
  (insert "# " topic "\n\n")
  (with-silent-modifications
    (put-text-property 1 (- (point) 1) 'read-only t))
  (markdown-mode)
  (spiel-compose-mode 1))

(defun spiel-prompt-with-region (start end)
  ""
  (interactive "r")
  (let ((topic (completing-read "Topic: " (spiel--list-topics)))
        (region-text (buffer-substring-no-properties start end)))
   (let ((prompt-buffer (get-buffer (spiel--prompt-buffer-name topic))))
      (unless prompt-buffer
        (spiel-prompt topic))
      (switch-to-buffer (get-buffer (spiel--prompt-buffer-name topic)))
      (goto-char (point-max))
      (insert "\n" region-text))))


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
