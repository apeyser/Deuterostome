(defconst dm-server "
(defun dm-server (name)
  (when (eq dm-server-process nil)
    (setq dm-server-name name)
    (dm-server-force-delete)
    (message nil)
    (dm-server-start)
    (add-hook 'kill-buffer-hook (lambda () (dm-server-start t)) t t)))
")
(defun build ()
  (catch 'ret
    (mapc 
     (lambda (dir)
       (let ((path (concat 
		    (file-name-as-directory 
		     (expand-file-name (if dir dir ".")))
		    "server.el")))
	 (when (file-readable-p path)
	   (set-buffer (get-buffer-create "*server*"))
	   (insert-file-contents path nil nil nil t)
	   (goto-char (point-min))
	   (while (search-forward "server" nil t)
	     (when (not (looking-back ":server"))
	       (replace-match "dm-server" nil t)))
	   (goto-char (point-max))
	   (insert dm-server)
	   (write-file output-file-name)
	   (throw 'ret nil))))
     load-path)))
