;;; kitten-capture --- Helper functions for opening a capture external to Emacs
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package org-mac-link)
(use-package noflet)

(defun kitten-capture-inbox ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (make-frame '((name . "capture")
                (top . 100)
                (left . 200)
                (width . 80)
                (height . 25)))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
          (org-capture nil "i")))

(defadvice kitten-capture-finalize (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice kitten-capture-destroy (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun kitten-capture-note-safari-url ()
  "Call `org-capture-string' on the current front most Safari window.
Use `org-mac-link-safari-get-frontmost-url' to capture url from Safari.
Triggered by a custom macOS Quick Action with a keyboard shortcut."
  (interactive)
  ;; (make-frame '((name . "capture")
  ;;               (top . 100)
  ;;               (left . 200)
  ;;               (width . 80)
  ;;               (height . 25)))
  ;; (select-frame-by-name "capture")
  ;; (delete-other-windows)
  (org-capture-string (org-mac-link-safari-get-frontmost-url) "u")
  (ignore-errors))

(defun kitten-capture-note-chrome-url ()
  "Call `org-capture-string' on the current front most Safari window.
Use `org-mac-link-safari-get-frontmost-url' to capture url from Safari.
Triggered by a custom macOS Quick Action with a keyboard shortcut."
  (interactive)
  ;; (make-frame '((name . "capture")
  ;;               (top . 100)
  ;;               (left . 200)
  ;;               (width . 80)
  ;;               (height . 25)))
  ;; (select-frame-by-name "capture")
  ;; (delete-other-windows)

  (org-capture-string (org-mac-link-chrome-get-frontmost-url) "u")
  (ignore-errors))

(defun kitten-capture-note-with-content (file)

  (org-capture-string "Contents:" "e")
  (insert-file-contents file))

(provide 'kitten-capture)

;;; kitten-capture.el ends here.
