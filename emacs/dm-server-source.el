; Copyright 2011 Alexander Peyser & Wolfgang Nonner
;
; This file is part of Deuterostome.
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(defconst dm-server-tail "
(defun dm-server (name)
  (when (eq dm-server-process nil)
    (setq dm-server-name name)
    (when (fboundp 'dm-server-force-delete) 
      (funcall 'dm-server-force-delete)
      (message nil))
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

(setq dm-server-fin "")
(defun dm-server-dir (dir)
  (setq path (concat (file-name-as-directory 
		      (expand-file-name 
		       (if dir dir default-directory)))
		     "server.el"
		     dm-server-fin))
  (when (file-readable-p path)
    (if (not (string= dm-server-fin ".gz"))
	(insert-file-contents path nil nil nil t)
      (unless (call-process gunzip path t)
	(error "Unable to ungzip %s" path)))
    (throw 'dm-server-build nil)))

(defun dm-server-build-do ()
  (set-buffer (get-buffer-create "*server*"))
  (catch 'dm-server-build
    (mapc 'dm-server-dir load-path)
    (setq dm-server-fin ".gz")
    (mapc 'dm-server-dir load-path)
    (error "Source file not found: %s (load-path: %s)" 
	   "server.el" load-path))
  (goto-char (point-min))
  (while (search-forward "server" nil t)
    (when (not (looking-back ":server"))
      (replace-match "dm-server" nil t)))
  (goto-char (point-min))
  (insert (dm-server-header path))
  (goto-char (point-max))
  (insert dm-server-tail)
  (write-file output-file-name))

(defun dm-server-build ()
  (condition-case err (dm-server-build-do)
    (error
     (message "%s" (error-message-string err))
     (kill-emacs 1))))
