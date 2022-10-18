(use-package a)

(require 'a)
(require 'bind-key)
(require 'seq)

(defvar reflex/signal-map nil
  "signal->mode->key+command")

;;(setq reflex/signal-map nil)

;; TODO call bind-key when implementation is available
;; if mode key is unset but global key is set, use global key
;; otherwise use mode key

(defun reflex/-install-signal-binding! (signal)
  (when-let ((mode->key (a-get reflex/signal-map signal)))
    (let ((global-key (a-get-in mode->key [global :key]))
	  (global-target (a-get-in mode->key [global :target])))
      (seq-map
       (lambda (x)
	 (let ((mode (car x))
	       (target (a-get (cdr x) :target))
	       (key (a-get (cdr x) :key)))
	   (let ((keymap (when (not (eq 'global mode))
                           (intern (concat (symbol-name mode) "-map")))))

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
       mode->key))))

;;(reflex/-install-signal-binding! :eval/last-sexp)
;;(reflex/-install-signal-binding! :eval/buffer)

(defun reflex/bind-signal (key signal &optional mode)
  (let ((mode (or mode 'global)))
    (setq reflex/signal-map (a-assoc-in reflex/signal-map (list signal mode :key) key))
    (reflex/-install-signal-binding! signal)))

(defun reflex/-bind-signals (mode bindings)
  (seq-map
   (lambda (x)
     (let ((key (car x))
           (signal (cadr x)))
       (reflex/bind-signal key signal mode)))
   bindings))

;;(reflex/-bind-signals
;; 'global
;; '(("C-c e f" :foo/bar)
;;   ("C-c e g" :foo/baz)))

(defmacro reflex/bind-signals (mode &rest bindings)
  (if (and (eq :prefix-map (car bindings))
           (eq :prefix (caddr bindings)))
      (let ((prefix-map (cadr bindings))
            (prefix (cadddr bindings)))
        `((defvar ,prefix-map)
          (define-prefix-command ',prefix-map)
          ;; TODO bind to prefix map (not mode) and then bind prefix key in mode
          (reflex/-bind-signals ',mode )))
    (list 'reflex/-bind-signals (list 'quote mode) (list 'quote bindings))))

;;(reflex/bind-signals
;; global
;; :prefix-map kitten/foo
;; :prefix "C-c t"
;; ("f" :foo/bar)
;; ("f" :foo/baz))

(defun reflex/provide-signal (signal target &optional mode)
  (let ((mode (or mode 'global)))
    (setq reflex/signal-map (a-assoc-in reflex/signal-map (list signal mode :target) target))
    (reflex/-install-signal-binding! signal)))

(defun reflex/-provide-signals (mode bindings)
  (seq-map
   (lambda (x)
     (let ((signal (car x))
           (target (cadr x)))
       (reflex/provide-signal signal target mode)))
   bindings))

(defmacro reflex/provide-signals (mode &rest signals)
  (list 'reflex/-provide-signals (list 'quote mode) (list 'quote signals)))

;;(reflex/bind-signal "C-c e b" :eval/buffer)
;;(reflex/bind-signal "C-c e e" :eval/last-sexp)
;;(reflex/provide-signal :eval/buffer #'eval-buffer 'emacs-lisp-mode)
;;(reflex/provide-signal :eval/last-sexp #'eval-last-sexp 'emacs-lisp-mode)
;;(reflex/provide-signal :eval/last-sexp #'cider-eval-last-sexp 'clojure-mode)
;;
;;(bind-key "C-c e e" #'eval-last-sexp emacs-lisp-mode-map)
;;(unbind-key "C-c e e" emacs-lisp-mode-map)
;;
;;(bind-key "C-c e b" #'eval-buffer emacs-lisp-mode-map)
;;(unbind-key "C-c e b" emacs-lisp-mode-map)

(provide 'reflex)
