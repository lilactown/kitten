;; Misc
(set-frame-font "Cascadia Mono PL-13")

;; Remove scroll bars when not needed and borders around them
(fringe-mode -1)

;; set frame borders
(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 0)))

(global-display-line-numbers-mode)
(column-number-mode)
(size-indication-mode)

;; don't constantly change the width of the buffer while scrolling
(setq display-line-numbers-grow-only t)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (setq doom-dracula-brighter-comments t)
  (setq doom-dracula-comment-bg nil)
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode 1))

;; dim buffers that aren't visiting a file
(use-package solaire-mode
  :init (solaire-global-mode +1))

;; show a dashboard on startup
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title "Welcome to kitten, the cutest feline Emacs")
  (setq dashboard-center-content t))

;; Enabling desktop-save-mode will save and restore all buffers between sessions
;(setq desktop-restore-frames nil)
;(desktop-save-mode 1)

;; start in full screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; hide menu bar
(menu-bar-mode -1)

;; hide toolbar
(tool-bar-mode -1)

(scroll-bar-mode -1)


;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so window
;; borders will match the enabled theme.
(and (or (daemonp)
         (display-graphic-p))
     (require 'ns-auto-titlebar nil t)
     (ns-auto-titlebar-mode +1))

(provide 'kitten-ui)
