;;; reflex.el --- Manage all your key bindings in one place

;;; Commentary:

;; reflex helps manage all your key bindings in one place by bindings keys to
;; _capabilities_, not functions.

;;; Code:

(require 'use-package)

(use-package a)

(require 'a)
(require 'bind-key)
(require 'seq)

(defvar reflex/signal-map nil
  "Global signal->keymap->key+command.")

(defun reflex/-install-signal-binding! (signal)
  "Look up SIGNAL in the global signal-map.  Internal only.
Binds all keys that appear in key."
  (when-let ((keymap->key (a-get reflex/signal-map signal)))
    (let ((global-key (a-get-in keymap->key [global :key]))
          (global-target (a-get-in keymap->key [global :target])))
      (seq-map
       (lambda (x)
         (let ((keymap (car x))
               (target (or (a-get (cdr x) :target) global-target))
               (key (or (a-get (cdr x) :key) global-key)))
           (let ((keymap (when (not (eq 'global keymap))
                           keymap)))

             (when (and target key)
               (bind-key key target keymap)))))
       keymap->key))))

(defun reflex/bind-signal (key signal &optional keymap)
  "Binds KEY to SIGNAL, with optional KEYMAP."
  (let ((keymap (or keymap 'global)))
    (setq reflex/signal-map (a-assoc-in reflex/signal-map (list signal keymap :key) key))
    (reflex/-install-signal-binding! signal)))

(defun reflex/-bind-signals (keymap bindings)
  (seq-map
   (lambda (x)
     (let ((key (car x))
           (signal (cadr x)))
       (reflex/bind-signal key signal keymap)))
   bindings))

(defmacro reflex/bind-signals (keymap &rest bindings)
  (list 'reflex/-bind-signals (list 'quote keymap) (list 'quote bindings)))

(defun reflex/provide-signal (signal target &optional keymap)
  (let ((keymap (or keymap 'global)))
    (setq reflex/signal-map (a-assoc-in reflex/signal-map (list signal keymap :target) target))
    (reflex/-install-signal-binding! signal)))

(defun reflex/-provide-signals (keymap bindings)
  (seq-map
   (lambda (x)
     (let ((signal (car x))
           (target (cadr x)))
       (reflex/provide-signal signal target keymap)))
   bindings))

(defmacro reflex/provide-signals (keymap &rest signals)
  (list 'reflex/-provide-signals (list 'quote keymap) (list 'quote signals)))

(provide 'reflex)

;;; reflex.el ends here
