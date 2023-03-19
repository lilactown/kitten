(use-package fuel
  :init
  (require 'factor-mode)
  (reflex/provide-signals
   factor-mode-map
   (:repl/jack-in run-factor)
   (:repl/connect connect-to-factor)
   (:repl/doc-apropos fuel-apropos)
   (:repl/switch-to fuel-switch-to-buffer-other-window)
   (:eval/defun fuel-eval-definition)
   (:eval/region fuel-eval-region)
   (:eval/buffer fuel-run-file))
  (setq fuel-listener-factor-binary
        "/Applications/factor/factor")
  (setq fuel-listener-factor-image
        "/Applications/factor/factor.image"))
