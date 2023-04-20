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
  ;; going through https://www.labri.fr/perso/nrougier/GTD/index.html
  (setq org-capture-templates
       `(("i" "Inbox" entry (file "roam/inbox.org")
          ,(concat "* TODO %?\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":END:"))))

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
        '(("n" "All todos" ((agenda "")
                            (todo)))
          ("p" "Personal"
           ((agenda ""
                    ((org-agenda-prefix-format " %i %-12:c%?-12t%-6e% s")
                     (org-agenda-scheduled-leaders '("" ""))))
            (tags-todo "+personal|+life"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'scheduled))
                   (org-agenda-prefix-format "  %i %-12:c [%e] "))))
           ((org-agenda-tag-filter '("-work"))))
          ("w" "Work"
           ((agenda "+work|+life"
                    ((org-agenda-prefix-format " %i %-12:c%?-12t%-6e% s")
                     (org-agenda-scheduled-leaders '("" ""))))
            (tags-todo "+work|+life"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")))))))

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
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  )


(use-package org-super-agenda
  :bind (:map org-super-agenda-header-map
              ("<tab>" . #'origami-toggle-node)
              ;; fix meow up / down and leader key when cursor on headings
              ("SPC" . nil)
              ("e" . nil))
  :config
  (org-super-agenda-mode)
  (setq org-agenda-span 'day)
  (setq org-super-agenda-groups
        '((:name "Today"
                 :time-grid t)
          (:name "Up next"
                 :todo "NEXT" :scheduled today)
          (:name "Must do"
                 :and (:tag "@urgent" :tag "@important"))
          (:name "Delegate"
                 :tag "@urgent")
          (:name "Schedule later"
                 :tag "@important")
          (:name "Bugs"
                 :category "bugs"))))


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
                            "#+title: %<%Y-%m-%d>\n"))))
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "Project" plain
           "%?"
           :if-new (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+STARTUP: content showstars indent\n#+FILETAGS: project\n#+PROPERTY: Effort_ALL 0 0:05 0:10 0:15 0:30 0:45 1:00 2:00 4:00\n#+TAGS: @urgent(u) @important(i)\n#+title: ${title}\n")
           :unnarrowed t)
          ("a" "Area" plain
           "%?"
           :if-new (file+head "areas/${slug}.org"
                              "#+STARTUP: content showstars indent\n#+FILETAGS: area\n#+PROPERTY: Effort_ALL 0 0:05 0:10 0:15 0:30 0:45 1:00 2:00 4:00\n#+TAGS: @urgent(u) @important(i)\n#+title: ${title}\n")
           :unnarrowed t)
          ("s" "Person" plain
           "%?"
           :if-new (file+head "people/${slug}.org"
                              "#+FILETAGS: person\n#+title: ${title}\n")
           :unnarrowed t)
          ("r" "Reference" plain "%?"
	   :target (file+head "refs/${slug}.org"
                              "#+TITLE: ${title}
#+CREATED: %u
#+FILETAGS: :work:reference:
* ${title}
:PROPERTIES:
:Type:
:Start:
:Fin:
:Creator:
:URL:
:END:
* Actions
* Key Ideas
** Quotes
** Notes
") :unnarrowed t))))


(use-package org-roam-ui
  :after org)

(defun kitten-org/update-tags (&optional arg)
  "Update tags of current visible entry ARG.
Hides doom-modeline while doing it."
  (interactive "P")
  (doom-modeline-mode -1)
  (org-set-tags-command arg)
  (doom-modeline-mode 1))

;; calendar framework
(use-package calfw
  :config
  (use-package calfw-org)
  (use-package calfw-blocks
    :config
    (defun kitten/calendar-agenda ()
      (interactive)
      (cfw:open-calendar-buffer
       :contents-sources
       (list
        (cfw:org-create-source "medium purple"))
       :view 'block-week))))


(reflex/provide-signals
 global
 (:notes/agenda org-agenda)
 (:notes/calendar kitten/calendar-agenda)
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
