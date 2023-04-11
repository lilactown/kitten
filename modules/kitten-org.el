;;; kitten-org --- Kitten org mode configuration

;;; Commentary:

;; Contains my custom note taking system, based on Getting Things Done.
;; Not everyone's cup of tea.  Feel free to not use.

;;; Code:

(require 'kitten-vars)
(require 'use-package)
(require 'bind-key)
(require 'reflex)

(use-package org
  :init
  (setq org-directory kitten-org-dir)
  (setq org-agenda-files kitten-org-agenda-files)

  ;; going through https://www.labri.fr/perso/nrougier/GTD/index.html
  (setq org-capture-templates
       `(("i" "Inbox" entry (file "inbox.org")
          ,(concat "* TODO %?\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":END:"))
         ("m" "Meeting" entry (file+headline "agenda.org" "Future")
           ,(concat "* %? :meeting:\n"
                    "<%<%Y-%m-%d %a %H:00>>"))
         ("n" "Note" entry  (file "notes.org")
          ,(concat "* Note (%a)\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":END:"
                   "\n" "%?"))
         ("u" "URL" entry (file "notes.org")
          ,(concat "* Note (%i)\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":END:"
                   "\n" "%?"))
         ("e" "External Content" entry (file "notes.org")
          ,(concat "* Note\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":END:"
                   "\n" "%?"
                   "%i"))))

  ;; TODO
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (defun log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

  (setq org-log-done 'time)

  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

  ;; Use full window for org-capture
  (add-hook 'org-capture-mode-hook 'delete-other-windows)
  (defun org-capture-inbox ()
    (interactive)
    (org-capture nil "i"))

  ;; set up refiling behavior
  (setq org-refile-targets
        '((org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  ;; agenda
  (setq org-agenda-custom-commands
        '(("p" "Prioritize"
           ((tags-todo "+@urgent+@important"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "Urgent & Important\n")))
            (tags-todo "+@urgent-@important"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "Urgent\n")))
            (tags-todo "+@important-@urgent"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "Important\n")))))
          ("b" "Backlog"
           ((tags-todo "-@important-@urgent"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "Backlog\n")))))
          ("g" "Get Things Done (GTD)"
           ((agenda ""
                    (;(org-agenda-skip-function
                     ;;'(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)
                     (org-agenda-prefix-format " %i %-12:c%?-12t%-6e% s")))
            (todo "NEXT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline 'scheduled))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "Tasks\n")))
            (todo "WAITING"
                  (;;(org-agenda-skip-function
                   ;;  '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "Waiting\n")) )
            (agenda nil
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                     (org-agenda-overriding-header "Deadlines\n")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "Inbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))))

  ;; Copied from https://github.com/minad/org-modern
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"
   org-hide-leading-stars t
   org-startup-indented t

   ;; Agenda styling
   org-agenda-tags-column 0
   ;; org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

(use-package org-roam
  ;; :straight (org-roam
  ;;             :fork (:host github :repo "lilactown/org-roam"))
  :after org
  :custom (org-roam-directory (concat (file-truename org-directory) "/roam"))
  :config (org-roam-setup)
  :init
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %a\n%?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n")))))


(use-package org-roam-ui
  :after org)

(defun kitten-org/update-tags (&optional arg)
  "Update tags of current visible entry ARG.
Hides doom-modeline while doing it."
  (interactive "P")
  (doom-modeline-mode -1)
  (org-set-tags-command arg)
  (doom-modeline-mode 1))

(reflex/provide-signals
 global
 (:notes/agenda org-agenda)
 (:notes/capture org-capture)
 (:notes/capture-daily org-roam-dailies-capture-today)
 (:notes/inbox org-capture-inbox)
 (:notes/capture-external org-mac-get-link)

 (:roam/find-node org-roam-node-find)
 (:roam/capture org-roam-capture)
 (:roam/insert-node org-roam-node-insert)
 (:roam/toggle org-roam-buffer-toggle)
 (:roam/create-id org-id-get-create)
 (:roam/extract org-roam-extract-subtree)
 (:roam/sync org-roam-db-sync)
 (:roam/daily org-roam-dailies-goto-today)

 (:notes/return org-meta-return)
 (:notes/clock-in org-clock-in)
 (:notes/clock-out org-clock-out)
 (:notes/insert-time-stamp org-time-stamp)
 (:notes/insert-time-stamp-inactive org-time-stamp-inactive)
 (:notes/refile org-refile)
 (:notes/update-cookies org-update-statistics-cookies)
 (:notes/update-deadline org-deadline)
 (:notes/update-effort org-set-effort)
 (:notes/update-schedule org-schedule)
 (:notes/update-tags kitten-org/update-tags))

(provide 'kitten-org)

;;; kitten-org.el ends here.
