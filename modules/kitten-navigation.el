;;; kitten-completion.el --- Package config for navigation in and between buffers
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'reflex)

;; This package provides visible, buffer local, bookmarks and the ability to
;; jump forward and backward to the next bookmark.
;; https://github.com/joodland/bm
(use-package bm
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)

  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)
  ;; I'm skipping all the saving/loading stuff for now
  )

(use-package avy)


(reflex/provide-signals
 global
 (:navigation/jump-char avy-goto-word-1)
 (:navigation/jump-search avy-goto-char-timer)
 (:navigation/bookmark-toggle bm-toggle)
 (:navigation/bookmark-next bm-next)
 (:navigation/bookmark-prev bm-previous))

(provide 'kitten-navigation)

;; kitten-navigation.el ends here
