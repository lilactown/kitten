(use-package a)

(require 'a)
(require 'bind-key)
(require 'seq)

(defvar reflex/signal-map nil
  "signal->keymap->key+command")

(defun reflex/-install-signal-binding! (signal)
  (when-let ((keymap->key (a-get reflex/signal-map signal)))
    (let ((global-key (a-get-in keymap->key [global :key]))
	  (global-target (a-get-in keymap->key [global :target])))
      (seq-map
       (lambda (x)
	 (let ((keymap (car x))
	       (target (a-get (cdr x) :target))
	       (key (a-get (cdr x) :key)))
	   (let ((keymap (when (not (eq 'global keymap))
                           keymap)))

             (cond
              ((and target key)
               ;;(print (list key target keymap))
               (bind-key key target keymap))

              ((and global-target key)
               ;;(print (list key target keymap))
               (bind-key key global-target keymap))

              ((and target global-key)
               ;;(print (list key target keymap))
               (bind-key global-key target keymap))))))
       keymap->key))))

(defun reflex/bind-signal (key signal &optional keymap)
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
  (if (and (eq :prefix-map (car bindings))
           (eq :prefix (caddr bindings)))
      (let ((prefix-map (cadr bindings))
            (prefix (cadddr bindings)))
        `((defvar ,prefix-map)
          (define-prefix-command ',prefix-map)
          ;; TODO bind to prefix map (not mode) and then bind prefix key in mode
          (reflex/-bind-signals ',keymap )))
    (list 'reflex/-bind-signals (list 'quote keymap) (list 'quote bindings))))

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
