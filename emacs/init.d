(setq load-path (append (list nil "/dm/emacs") load-path))
(setenv "TERM" "ansi")

;; load local functions
;;(require 'my-buffer-funcs)
(require 'd-mode)

(set-face-foreground 'd-mode-oref-face "olivedrab")
(set-face-bold-p 'd-mode-oref-face nil)

(require 'd-comint-mode)
