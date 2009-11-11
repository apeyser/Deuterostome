(require 'server)

(defun dm-server-get-default (sym-name default sym-default)
  (let ((new-sym (intern-soft (format "%s-default" sym-name))))
    (if (and new-sym (boundp new-sym)) (symbol-value new-sym)
      (if default (symbol-value sym-default)))))

(defun dm-server-make-sym (sym check-bound default)
  (when (boundp sym)
    (let ((sym-name (symbol-name sym)))
      (let* ((dm-sym-name (format "dm-%s" sym-name))
	     (dm-sym (intern dm-sym-name)))
	(when (not (and check-bound (boundp dm-sym)))
	  (make-local-variable sym)
	  (set sym (dm-server-get-default dm-sym-name default sym))
	  (defvaralias dm-sym sym))))))

(defun dm-server-get-syms ()
  (mapatoms 
   (lambda (sym)
     (when (string-match "^server\\(-.*\\)?$" (symbol-name sym))
       (dm-server-make-sym sym t nil)))))

(defun dm-server-set-syms (&rest syms)
  (mapc (lambda (sym) (dm-server-make-sym sym nil t)) syms))

(defvar   dm-server-start                    nil)
(defconst dm-server-buffer-default           " *dm-server*")
(defconst dm-server-raise-frame-default      t)
(defconst dm-server-kill-new-buffers-default t)
(defvar   dm-server-name                     nil)
(defvar   dm-server-process                  nil)

;; server-raise-frame server-kill-new-buffers
(defun dm-server-start (nm)
  (when (not dm-server-start)
    (dm-server-set-syms
     'server-auth-dir 
     'server-socket-dir
     'server-log-time-function
     'server-temp-file-regexp
     'server-raise-frame
     'server-kill-new-buffers
     'server-name 
     'server-process)
    (dm-server-get-syms)
    (setq dm-server-start t))
   
  (when (not dm-server-process)
    (setq dm-server-name nm)
    (server-start)))

(provide 'dm-server)
