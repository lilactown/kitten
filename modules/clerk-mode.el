;;; clerk-mode.el --- Evaluate Clojure buffers and present them in clerk

;; Author: Will Acton
;; Version: 0.1
;; Keywords: tools
;; Package-Requires: ((cider))

;; This file is NOT part of GNU Emacs.

;; clerk-mode.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; clerk-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cljstyle-mode.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Present Clojure buffers in clerk

;;; Code:

(require 'cider)

(defun clerk-show ()
  "Present the current buffer (if visiting a file) in the clerk viewer."
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

;;;###autoload
(define-minor-mode clerk-mode
  "Minor mode for presenting files in Clerk."
  :lighter " clerk"
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "M-RET") 'clerk-show)
            keymap))

(provide 'clerk-mode)

;;; clerk-mode.el ends here
