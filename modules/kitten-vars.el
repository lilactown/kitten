;;; kitten-vars --- Common customizations for kitten modules
;;; Commentary:
;;; Code:

(defvar kitten-user-dir (expand-file-name "user" user-emacs-directory)
  "Location for kitten user configuration files.")

(defvar kitten-org-dir (expand-file-name "~/org")
  "Location for \"org-mode\" files.")
(defvar kitten-org-agenda-files
  (list "inbox.org" "agenda.org" "notes.org" "projects.org" "events.org")
  "Files to add to \"org-agenda\".")

(provide 'kitten-vars)

;;; kitten-vars.el ends here
