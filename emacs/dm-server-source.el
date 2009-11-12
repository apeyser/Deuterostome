;;(require 'server)

;; (defun dm-server-get-default (sym-name default sym-default)
;;   (let ((new-sym (intern-soft (format "%s-default" sym-name))))
;;     (if (and new-sym (boundp new-sym)) (symbol-value new-sym)
;;       (if default (symbol-value sym-default)))))

;; (defun dm-server-make-sym (sym check-bound default)
;;   (when (boundp sym)
;;     (let ((sym-name (symbol-name sym)))
;;       (let* ((dm-sym-name (format "dm-%s" sym-name))
;; 	     (dm-sym (intern dm-sym-name)))
;; 	(when (not (and check-bound (boundp dm-sym)))
;; 	  ;;(put sym 'permanent-local t)
;; 	  ;;(make-local-variable sym)
;; 	  (set sym (dm-server-get-default dm-sym-name default sym))
;; 	  ;;(put dm-sym 'permanent-local t)
;; 	  ;;(make-local-variable dm-sym)
;; 	  (defvaralias dm-sym sym))))))

;; (defun dm-server-get-syms ()
;;   (mapatoms 
;;    (lambda (sym)
;;      (when (string-match "^server\\(-.*\\)?$" (symbol-name sym))
;;        (dm-server-make-sym sym t nil)))))

;; (defun dm-server-set-syms (&rest syms)
;;   (mapc (lambda (sym) (dm-server-make-sym sym nil t)) syms))

;; (defvar   dm-server-start                    nil)
;; (make-variable-buffer-local 'dm-server-start)

;; (defconst dm-server-buffer-default           " *dm-server*")
;; (defconst dm-server-raise-frame-default      t)
;; (defconst dm-server-kill-new-buffers-default t)
;; (defvar   dm-server-name                     nil)
;; (defvar   dm-server-process                  nil)

(defconst dm-server "
(defun dm-server (name)
  (when (eq dm-server-process nil)
    (setq dm-server-name name)
    (dm-server-force-delete)
    (message nil)
    (dm-server-start)
    ;;(process-put dm-server-process :dm-server-buffer (current-buffer))
    (add-hook 'kill-buffer-hook (lambda () (dm-server-start t)) t t)))
")

(defun build ()
  (catch 'ret
    (mapc 
     (lambda (dir)
       (let ((path (concat (if dir dir ".") "/server.el")))
	 (when (file-readable-p path)
	   (save-excursion
	     (set-buffer (get-buffer-create "*server*"))
	     (insert-file-contents path nil nil nil t)
	     (goto-char (point-min))
	     (while (re-search-forward "server" nil t)
	       (when (not (looking-back ":server"))
		 (replace-match "dm-server" nil t)))
	     (goto-char (point-max))
	     (insert dm-server)
             (write-file output-file-name)
	     (throw 'ret nil)))))
     load-path)))

;; server-raise-frame server-kill-new-buffers
;;(defun dm-server (name)
  ;; (when (not dm-server-start)
  ;;   (dm-server-set-syms
  ;;    'server-auth-dir 
  ;;    'server-socket-dir
  ;;    'server-log-time-function
  ;;    'server-temp-file-regexp
  ;;    'server-raise-frame
  ;;    'server-kill-new-buffers
  ;;    'server-name 
  ;;    'server-process)
  ;;   (dm-server-get-syms)
  ;;   (setq dm-server-start t))
   
  ;; (when (eq dm-server-process nil)
  ;;   (setq dm-server-name name)
  ;;   (dm-server-force-delete)
  ;;   (message nil)
  ;;   (dm-server-start)
  ;;   ;;(process-put dm-server-process :dm-server-buffer (current-buffer))
  ;;   (add-hook 'kill-buffer-hook (lambda () (dm-server-start t)) t t)))

;; (defadvice server-sentinel (around 
;; 			    dm-server-sentinel 
;; 			    activate 
;; 			    compile 
;; 			    preactivate)
;;   (let ((buffer (process-get proc :dm-server-buffer)))
;;     (if buffer (save-excursion (set-buffer buffer) ad-do-it)
;;       ad-do-it)))

;; (defadvice server-process-filter (around 
;; 				  dm-server-process-filter
;; 				  activate 
;; 				  compile 
;; 				  preactivate)
;;   (let ((buffer (process-get proc :dm-server-buffer)))
;;     (if buffer (save-excursion (set-buffer buffer) ad-do-it)
;;       ad-do-it)))

;;(defun dm-server-debug (msg obj)
;;  (print msg)
;;  (print obj))

;; (fset 'dm-server-sentinel-old (symbol-function 'server-sentinel))
;; (defun server-sentinel (proc msg)
;;   (let ((buffer (process-get proc :dm-server-buffer)))
;;     (if buffer 
;; 	(save-excursion 
;; 	  (dm-server-debug "here" buffer) 
;; 	  (set-buffer buffer) 
;; 	  (dm-server-sentinel-old proc msg))
;;       (dm-server-debug "there" nil)
;;       (dm-server-sentinel-old proc msg))))

;; (fset 'dm-server-process-filter-old (symbol-function 'server-process-filter))
;; (defun server-process-filter (proc string)
;;   (let ((buffer (process-get proc :dm-server-buffer)))
;;     (if buffer 
;; 	(save-excursion 
;; 	  (dm-server-debug "here2" buffer) 
;; 	  (set-buffer buffer) 
;; 	  (dm-server-process-filter-old proc string))
;;       (dm-server-debug "there2" nil)
;;       (dm-server-process-filter-old proc string))))

;;(provide 'dm-server)
