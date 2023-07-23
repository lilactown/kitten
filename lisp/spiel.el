;; spiel.el --- An interface for ChatGPT  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package openai
  :straight (:type git :host github :repo "lilactown/openai"))


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

(defun spiel--say (topic message)
  ""
  (interactive)
  (let ((messages (vconcat (spiel--read-messages topic)
                           `[((role . "user")
                              (content . ,message))])))
    (openai-chat
     messages
     (lambda (data)
       (let ((choices (let-alist data .choices)))
         (with-output-to-temp-buffer (concat "*Topic: " topic "*")
           (mapc (lambda (message)
                   (let-alist message
                     (princ (format "%s: %s\n\n" .role (string-trim .content)))))
                 messages)
           (mapc (lambda (choice)
                   (let-alist choice
                     (let-alist .message
                       (princ (format "%s: %s\n\n" .role (string-trim .content))))))
                 choices))
         (spiel--write-messages
          topic
          (vconcat messages
                   (cl-map 'vector
                        (lambda (choice)
                          (let-alist choice .message))
                        choices)))))
     :parameters '(("api-version" . "2023-05-15")))))



;; (spiel--say "testing" "how are you?")
;; (spiel--say "testing" "pretend you're my grandma who loves me and answer the question.")
;; (spiel--say "testing" "Now pretend you're my son and answer the question.")


(defvar-keymap spiel-compose-mode-map
  :doc "Key map used by `spiel-compose-mode'.")

(define-minor-mode spiel-compose-mode
  "Minor mode used when composing a new message to send to a *spiel* topic."
  :keymap spiel-compose-mode-map)

(defun spiel-prompt (topic)
  ""
  (interactive "s")
  (switch-to-buffer (generate-new-buffer-name (concat "*Prompt (" topic ")*")))
  (org-mode)
  (spiel-compose-mode))


;; (spiel-prompt "testing")

(provide 'spiel)

;;; spiel.el ends here
