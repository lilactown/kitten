;; recommended by straight.el to avoid loading packages init files on load
(setq package-enable-at-startup nil)

;; https://www.reddit.com/r/emacs/comments/xfhnzz/weird_errors_with_latest_build_of_emacs/
(when (eq system-type 'darwin) (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))


;; Skip garbage collections during startup to speed things up. This is optional
;; but nice to have.

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 33554432 ; 32mb
                  gc-cons-percentage 0.1)))
