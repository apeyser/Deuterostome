
;;;### (autoloads (dvt d-comint-mode) "d-comint-mode" "ap/d-comint-mode.el"
;;;;;;  (16095 33481))
;;; Generated autoloads from ap/d-comint-mode.el

(autoload (quote d-comint-mode) "d-comint-mode" "\
Major mode for handling d-machine" t nil)

(autoload (quote dvt) "d-comint-mode" "\
Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)" t nil)

;;;***

;;;### (autoloads (d-mode) "d-mode" "ap/d-mode.el" (16095 34166))
;;; Generated autoloads from ap/d-mode.el

(setq auto-mode-alist (append (quote (("\\.[dD]\\'" . d-mode))) auto-mode-alist))

(autoload (quote d-mode) "d-mode" "\
Major mode for editing d code." t nil)

;;;***

;;;### (autoloads (dvt d-comint-mode) "d-comint-mode" "ap/d-comint-mode.el"
;;;;;;  (16095 30377))
;;; Generated autoloads from ap/d-comint-mode.el

(autoload (quote d-comint-mode) "d-comint-mode" "\
Major mode for handling d-machine" t nil)

(autoload (quote dvt) "d-comint-mode" "\
Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)" t nil)

;;;***

(font-lock-mode 1)
;;(add-hook 'd-mode-hook (lambda () (font-lock-mode 1)))

(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(setq generated-autoload-file "/sw/etc/emacs/site-lisp/site-start.el")

(setq tex-dvi-print-command "dviToLW.sh")
(setq tex-dvi-view-command "xdvi")

(setq lpr-command "toLW.sh")
(setq lpr-printer-switch "")
(setq printer-name "")

(setq ps-lpr-command "PStoLW.sh")
(setq ps-printer-name-option "")
(setq ps-printer-name "")

(defun my-buffer-name-headers () (concat "-H \"" (buffer-name) "\""))
(setq lpr-headers-switches '(my-buffer-name-headers))

(require 'tex-site)
(setq load-path (append load-path '("/sw/etc/emacs/ap")))
