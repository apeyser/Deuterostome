(defconst dm-server-tail "
(defun dm-server (name)
  (when (eq dm-server-process nil)
    (setq dm-server-name name)
    (dm-server-force-delete)
    (message nil)
    (dm-server-start)
    (add-hook 'kill-buffer-hook (lambda () (dm-server-start t)) t t)))
")

(defconst dm-server-header-format "\
;; Automatically built on %s
;;   from %s

(defconst dm-server-build-timestamp \"%s\")
(defconst dm-server-build-path \"%s\")

")

(defun dm-server-header (path)
  (format dm-server-header-format
	  (format-time-string "%Y-%m-%d %H:%M:%S %z") 
	  path
	  (format-time-string "%Y-%m-%d %H:%M:%S %z")
	  path))

(defun dm-server-build ()
  (let ((path 
	 (catch 'dm-server-build
	   (mapc 
	    (lambda (dir)
	      (let ((path 
		     (concat (file-name-as-directory 
			      (expand-file-name 
			       (if dir dir ".")))
			     "server.el")))
		(when (file-readable-p path)
		  (throw 'dm-server-build path))))
	    load-path)
	   (error "Source file not found: %s (load-path: %s)" 
		  "server.el" load-path))))
    (set-buffer (get-buffer-create "*server*"))
    (insert-file-contents path nil nil nil t)
    (goto-char (point-min))
    (while (search-forward "server" nil t)
      (when (not (looking-back ":server"))
	(replace-match "dm-server" nil t)))
    (goto-char (point-min))
    (insert (dm-server-header path))
    (goto-char (point-max))
    (insert dm-server-tail)
    (write-file output-file-name)))
