(use-package org
  :init
  ;; going through https://www.labri.fr/perso/nrougier/GTD/index.html
  (setq org-directory "~/org")
  (setq org-agenda-files (list "inbox.org" "agenda.org"))
  (setq org-capture-templates
       `(("i" "Inbox" entry (file "inbox.org")
          ,(concat "* TODO %?\n"
                   "/Entered on/ %U"))
         ("m" "Meeting" entry (file+headline "agenda.org" "Future")
           ,(concat "* %? :meeting:\n"
                    "<%<%Y-%m-%d %a %H:00>>"))
         ("n" "Note" entry  (file "notes.org")
          ,(concat "* Note (%a)\n"
                   "/Entered on/ %U\n" "\n" "%?"))))

  ;; Use full window for org-capture
  (add-hook 'org-capture-mode-hook 'delete-other-windows)
  (defun org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i")))
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


;; (use-package org-roam
;;   :custom
;;   (org-roam-directory (file-truename "~/org-roam"))
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          ;; Dailies
;;          ("C-c n j" . org-roam-dailies-capture-today))
;;   :config
;;   ;; If you're using a vertical completion framework, you might want a more informative completion interface
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode))

(provide 'kitten-org)
