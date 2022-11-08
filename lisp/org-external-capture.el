;;; org-external-capture --- Helper functions for opening a capture external to Emacs
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package noflet)

(defun org-external-capture-inbox ()
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

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun org-external-capture-note-safari ()
  "Call `org-capture-string' on the current front most Safari window.
Use `org-mac-link-safari-get-frontmost-url' to capture url from Safari.
Triggered by a custom macOS Quick Action with a keyboard shortcut."
  (interactive)
    (make-frame '((name . "capture")
                (top . 100)
                (left . 200)
                (width . 80)
                (height . 25)))
  (select-frame-by-name "capture")
  (delete-other-windows)

  (org-capture-string (org-mac-link-safari-get-frontmost-url) "e")
  (ignore-errors))

(defun org-external-capture-note-chrome ()
  "Call `org-capture-string' on the current front most Safari window.
Use `org-mac-link-safari-get-frontmost-url' to capture url from Safari.
Triggered by a custom macOS Quick Action with a keyboard shortcut."
  (interactive)
    (make-frame '((name . "capture")
                (top . 100)
                (left . 200)
                (width . 80)
                (height . 25)))
  (select-frame-by-name "capture")
  (delete-other-windows)

  (org-capture-string (org-mac-link-chrome-get-frontmost-url) "e")
  (ignore-errors))

(provide 'org-external-capture)

;;; org-external-capture.el ends here.
