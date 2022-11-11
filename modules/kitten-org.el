;;; kitten-org --- Kitten org mode configuration

;;; Commentary:

;; Contains my custom note taking system, based on Getting Things Done.
;; Not everyone's cup of tea.  Feel free to not use.

;;; Code:

(require 'use-package)
(require 'bind-key)
(require 'reflex)

(use-package org
  :init
  ;; going through https://www.labri.fr/perso/nrougier/GTD/index.html
  (setq org-capture-templates
       `(("i" "Inbox" entry (file "inbox.org")
          ,(concat "* TODO %?\n"
                   "/Entered on/ %U"))
         ("m" "Meeting" entry (file+headline "agenda.org" "Future")
           ,(concat "* %? :meeting:\n"
                    "<%<%Y-%m-%d %a %H:00>>"))
         ("n" "Note" entry  (file "notes.org")
          ,(concat "* Note (%a)\n"
                   "/Entered on/ %U\n" "\n" "%?"))
         ("u" "URL" entry (file "notes.org")
          ,(concat "* Note (%i)\n"
                   "/Entered on/ %U\n" "\n" "%?"))
         ("e" "External Content" entry (file "notes.org")
          ,(concat "* Note\n"
                   "/Entered on/ %U\n" "\n" "%?"
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
        '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  ;; agenda
  (setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  (;(org-agenda-skip-function
                   ;;'(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)
                   (org-agenda-prefix-format " %i %-12:c%?-12t%-6e% s")))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline 'scheduled))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (todo "WAITING"
               (;;(org-agenda-skip-function
                ;;  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nWaiting\n")) )
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n"))))))))


(use-package org-modern
  :init
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (global-org-modern-mode))


(defvar kitten-mode/org (make-sparse-keymap))
(define-prefix-command 'kitten-mode/org)

(bind-keys
 :map kitten-mode/org
 ("RET" . org-meta-return)
 ("c i" . org-clock-in)
 ("c o" . org-clock-out)
 ("r" . org-refile)
 ("u c" . org-update-statistics-cookies)
 ("u d" . org-deadline)
 ("u e" . org-set-effort)
 ("u s" . org-schedule)
 ("u t" . (lambda (&optional arg)
            (interactive "P")
            (doom-modeline-mode -1)
            (org-set-tags-command arg)
            (doom-modeline-mode 1))))

(reflex/provide-signal :mode/major kitten-mode/org org-mode-map)

(provide 'kitten-org)

;;; kitten-org.el ends here.
