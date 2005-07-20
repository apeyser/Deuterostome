(define-prefix-command 'my-prefix)

(global-set-key [?\C-\\] my-prefix)

(global-set-key [?\C-\\ ?\C-e] 
  (lambda () "" (interactive) (cvs-examine "." ())))
(global-set-key [?\C-\\ ?\C-c] 'compile)
(global-set-key [?\C-\\ ?\C-r] 
  (lambda () "" (interactive) (revert-buffer t t t)))
(global-set-key [?\C-\\ ?\C-s] 'c-mode)
(global-set-key [?\C-\\ ?\C-f] 'font-lock-fontify-buffer)
(global-set-key [?\C-\\ ?\C-k] (lambda () "" (interactive) (kill-buffer nil)))
(global-set-key [?\C-\\ ?\C-g] 'grep)
(global-set-key [?\C-\\ ?e] 
  (lambda () "" (interactive) (cvs-examine "." ())))
(global-set-key [?\C-\\ ?c] 'compile)
(global-set-key [?\C-\\ ?r] 
  (lambda () "" (interactive) (revert-buffer t t t)))
(global-set-key [?\C-\\ ?s] 'c-mode)
(global-set-key [?\C-\\ ?f] 'font-lock-fontify-buffer)
(global-set-key [?\C-\\ ?k] (lambda () "" (interactive) (kill-buffer nil)))
(global-set-key [?\C-\\ ?g] 'grep)
(global-set-key [?\C-\\ (home)] 'beginning-of-buffer)
(global-set-key [?\C-\\ (end)] 'end-of-buffer)
(global-set-key [?\C-\\ ?/] 'undo)
(global-set-key [?\C-\\ ?\C-/] 'undo)
(global-set-key [?\C-\\ ?d] 'dvt)
(global-set-key [?\C-\\ ?n] 'dnode-mode)
(global-set-key [?\C-\\ ?m] 'dnode-kill)

(global-set-key [?\M-g] 'goto-line)

(defun dnode-mode (node)
  ""
  (interactive "p")
  (let ((explicit-shell-file-name "dnode.sh"))
    (dnode-shell node (concat "*dnode-" (number-to-string node) "*" ))))

(defun dnode-shell (node buffer)
  (if (not (comint-check-proc buffer))
      (let* ((prog )
	     (name (file-name-nondirectory prog))
	     (startfile (concat "~/.emacs_" name))
	     shell-buffer)
	(save-excursion
	  (set-buffer (apply 'make-comint-in-buffer "dnode" buffer prog
			     (if (file-exists-p startfile) startfile)
			     (number-to-string node)))
		      (setq shell-buffer (current-buffer))
	  (shell-mode))
	(pop-to-buffer shell-buffer))
    (pop-to-buffer buffer)))


(defun dnode-kill ()
  ""
  (interactive)
  (shell-command "killall dnode"))

(require 'cc-mode)

(defun my-c-key-bindings-hook ()
  (define-key c-mode-base-map [?\C-\\ ?\C-s] 'fundamental-mode)
  (define-key c-mode-base-map [?\C-\\ ?s] 'fundamental-mode))
(add-hook 'c-mode-common-hook 'my-c-key-bindings-hook)

(provide 'my-prefix)
