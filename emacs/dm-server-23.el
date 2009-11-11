;;; dm-server.el --- Lisp code for GNU Emacs running as server process

;; Copyright (C) 1986, 1987, 1992, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
;;   2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

;; Author: William Sommerfeld <wesommer@athena.mit.edu>
;; Maintainer: FSF
;; Keywords: processes

;; Changes by peck@sun.com and by rms.
;; Overhaul by Karoly Lorentey <lorentey@elte.hu> for multi-tty support.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This Lisp code is run in Emacs when it is to operate as
;; a server for other processes.

;; Load this library and do M-x server-edit to enable Emacs as a server.
;; Emacs opens up a socket for communication with clients.  If there are no
;; client buffers to edit, server-edit acts like (switch-to-buffer
;; (other-buffer))

;; When some other program runs "the editor" to edit a file,
;; "the editor" can be the Emacs client program ../lib-src/emacsclient.
;; This program transmits the file names to Emacs through
;; the server subprocess, and Emacs visits them and lets you edit them.

;; Note that any number of clients may dispatch files to Emacs to be edited.

;; When you finish editing a Server buffer, again call server-edit
;; to mark that buffer as done for the client and switch to the next
;; Server buffer.  When all the buffers for a client have been edited
;; and exited with server-edit, the client "editor" will return
;; to the program that invoked it.

;; Your editing commands and Emacs's display output go to and from
;; the terminal in the usual way.  Thus, server operation is possible
;; only when Emacs can talk to the terminal at the time you invoke
;; the client.  This is possible in four cases:

;; 1. On a window system, where Emacs runs in one window and the
;; program that wants to use "the editor" runs in another.

;; 2. On a multi-terminal system, where Emacs runs on one terminal and the
;; program that wants to use "the editor" runs on another.

;; 3. When the program that wants to use "the editor" is running
;; as a subprocess of Emacs.

;; 4. On a system with job control, when Emacs is suspended, the program
;; that wants to use "the editor" will stop and display
;; "Waiting for Emacs...".  It can then be suspended, and Emacs can be
;; brought into the foreground for editing.  When done editing, Emacs is
;; suspended again, and the client program is brought into the foreground.

;; The buffer local variable "server-buffer-clients" lists
;; the clients who are waiting for this buffer to be edited.
;; The global variable "server-clients" lists all the waiting clients,
;; and which files are yet to be edited for each.

;; Todo:

;; - handle command-line-args-left.
;; - move most of the args processing and decision making from emacsclient.c
;;   to here.
;; - fix up handling of the client's environment (place it in the terminal?).

;;; Code:

(eval-when-compile (require 'cl))

(defgroup dm-server nil
  "Emacs running as a dm-server process."
  :group 'external)

(defcustom dm-server-use-tcp nil
  "If non-nil, use TCP sockets instead of local sockets."
  :set #'(lambda (sym val)
           (unless (featurep 'make-network-process '(:family local))
             (setq val t)
             (unless load-in-progress
               (message "Local sockets unsupported, using TCP sockets")))
           (when val (random t))
           (set-default sym val))
  :group 'dm-server
  :type 'boolean
  :version "22.1")

(defcustom dm-server-host nil
  "The name or IP address to use as host address of the dm-server process.
If set, the dm-server accepts remote connections; otherwise it is local."
  :group 'dm-server
  :type '(choice
          (string :tag "Name or IP address")
          (const :tag "Local" nil))
  :version "22.1")
(put 'dm-server-host 'risky-local-variable t)

(defcustom dm-server-auth-dir (locate-user-emacs-file "dm-server/")
  "Directory for dm-server authentication files."
  :group 'dm-server
  :type 'directory
  :version "22.1")
(put 'dm-server-auth-dir 'risky-local-variable t)

(defcustom dm-server-raise-frame t
  "If non-nil, raise frame when switching to a buffer."
  :group 'dm-server
  :type 'boolean
  :version "22.1")

(defcustom dm-server-visit-hook nil
  "Hook run when visiting a file for the Emacs dm-server."
  :group 'dm-server
  :type 'hook)

(defcustom dm-server-switch-hook nil
  "Hook run when switching to a buffer for the Emacs dm-server."
  :group 'dm-server
  :type 'hook)

(defcustom dm-server-done-hook nil
  "Hook run when done editing a buffer for the Emacs dm-server."
  :group 'dm-server
  :type 'hook)

(defvar dm-server-process nil
  "The current dm-server process.")

(defvar dm-server-clients nil
  "List of current dm-server clients.
Each element is a process.")

(defvar dm-server-buffer-clients nil
  "List of client processes requesting editing of current buffer.")
(make-variable-buffer-local 'dm-server-buffer-clients)
;; Changing major modes should not erase this local.
(put 'dm-server-buffer-clients 'permanent-local t)

(defcustom dm-server-window nil
  "Specification of the window to use for selecting Emacs dm-server buffers.
If nil, use the selected window.
If it is a function, it should take one argument (a buffer) and
display and select it.  A common value is `pop-to-buffer'.
If it is a window, use that.
If it is a frame, use the frame's selected window.

It is not meaningful to set this to a specific frame or window with Custom.
Only programs can do so."
  :group 'dm-server
  :version "22.1"
  :type '(choice (const :tag "Use selected window"
			:match (lambda (widget value)
				 (not (functionp value)))
			nil)
		 (function-item :tag "Display in new frame" switch-to-buffer-other-frame)
		 (function-item :tag "Use pop-to-buffer" pop-to-buffer)
		 (function :tag "Other function")))

(defcustom dm-server-temp-file-regexp "^/tmp/Re\\|/draft$"
  "Regexp matching names of temporary files.
These are deleted and reused after each edit by the programs that
invoke the Emacs dm-server."
  :group 'dm-server
  :type 'regexp)

(defcustom dm-server-kill-new-buffers t
  "Whether to kill buffers when done with them.
If non-nil, kill a buffer unless it already existed before editing
it with the Emacs dm-server.  If nil, kill only buffers as specified by
`dm-server-temp-file-regexp'.
Please note that only buffers that still have a client are killed,
i.e. buffers visited with \"emacsclient --no-wait\" are never killed
in this way."
  :group 'dm-server
  :type 'boolean
  :version "21.1")

(or (assq 'dm-server-buffer-clients minor-mode-alist)
    (push '(dm-server-buffer-clients " Dm-Server") minor-mode-alist))


(defvar dm-server-existing-buffer nil
  "Non-nil means the buffer existed before the dm-server was asked to visit it.
This means that the dm-server should not kill the buffer when you say you
are done with it in the dm-server.")
(make-variable-buffer-local 'dm-server-existing-buffer)

(defcustom dm-server-name "dm-server"
  "The name of the Emacs dm-server, if this Emacs process creates one.
The command `dm-server-start' makes use of this.  It should not be
changed while a dm-server is running."
  :group 'dm-server
  :type 'string
  :version "23.1")

;; We do not use `temporary-file-directory' here, because emacsclient
;; does not read the init file.
(defvar dm-server-socket-dir
  (and (featurep 'make-network-process '(:family local))
       (format "%s/emacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid)))
  "The directory in which to place the dm-server socket.
If local sockets are not supported, this is nil.")

(defun dm-server-clients-with (property value)
  "Return a list of clients with PROPERTY set to VALUE."
  (let (result)
    (dolist (proc dm-server-clients result)
      (when (equal value (process-get proc property))
	(push proc result)))))

(defun dm-server-add-client (proc)
  "Create a client for process PROC, if it doesn't already have one.
New clients have no properties."
  (add-to-list 'dm-server-clients proc))

(defmacro dm-server-with-environment (env vars &rest body)
  "Evaluate BODY with environment variables VARS set to those in ENV.
The environment variables are then restored to their previous values.

VARS should be a list of strings.
ENV should be in the same format as `process-environment'."
  (declare (indent 2))
  (let ((var (make-symbol "var"))
	(value (make-symbol "value")))
    `(let ((process-environment process-environment))
       (dolist (,var ,vars)
         (let ((,value (getenv-internal ,var ,env)))
           (push (if (stringp ,value)
                     (concat ,var "=" ,value)
                   ,var)
                 process-environment)))
       (progn ,@body))))

(defun dm-server-delete-client (proc &optional noframe)
  "Delete PROC, including its buffers, terminals and frames.
If NOFRAME is non-nil, let the frames live.
Updates `dm-server-clients'."
  (dm-server-log (concat "dm-server-delete-client" (if noframe " noframe")) proc)
  ;; Force a new lookup of client (prevents infinite recursion).
  (when (memq proc dm-server-clients)
    (let ((buffers (process-get proc 'buffers)))

      ;; Kill the client's buffers.
      (dolist (buf buffers)
	(when (buffer-live-p buf)
	  (with-current-buffer buf
	    ;; Kill the buffer if necessary.
	    (when (and (equal dm-server-buffer-clients
			      (list proc))
		       (or (and dm-server-kill-new-buffers
				(not dm-server-existing-buffer))
			   (dm-server-temp-file-p))
		       (not (buffer-modified-p)))
	      (let (flag)
		(unwind-protect
		    (progn (setq dm-server-buffer-clients nil)
			   (kill-buffer (current-buffer))
			   (setq flag t))
		  (unless flag
		    ;; Restore clients if user pressed C-g in `kill-buffer'.
		    (setq dm-server-buffer-clients (list proc)))))))))

      ;; Delete the client's frames.
      (unless noframe
	(dolist (frame (frame-list))
	  (when (and (frame-live-p frame)
		     (equal proc (frame-parameter frame 'client)))
	    ;; Prevent `dm-server-handle-delete-frame' from calling us
	    ;; recursively.
	    (set-frame-parameter frame 'client nil)
	    (delete-frame frame))))

      (setq dm-server-clients (delq proc dm-server-clients))

      ;; Delete the client's tty.
      (let ((terminal (process-get proc 'terminal)))
	;; Only delete the terminal if it is non-nil.
	(when (and terminal (eq (terminal-live-p terminal) t))
	  (delete-terminal terminal)))

      ;; Delete the client's process.
      (if (eq (process-status proc) 'open)
	  (delete-process proc))

      (dm-server-log "Deleted" proc))))

(defvar dm-server-log-time-function 'current-time-string
  "Function to generate timestamps for `dm-server-buffer'.")

(defconst dm-server-buffer " *dm-server*"
  "Buffer used internally by Emacs's dm-server.
One use is to log the I/O for debugging purposes (see `dm-server-log'),
the other is to provide a current buffer in which the process filter can
safely let-bind buffer-local variables like `default-directory'.")

(defvar dm-server-log nil
  "If non-nil, log the dm-server's inputs and outputs in the `dm-server-buffer'.")

(defun dm-server-log (string &optional client)
  "If `dm-server-log' is non-nil, log STRING to `dm-server-buffer'.
If CLIENT is non-nil, add a description of it to the logged message."
  (when dm-server-log
    (with-current-buffer (get-buffer-create dm-server-buffer)
      (goto-char (point-max))
      (insert (funcall dm-server-log-time-function)
	      (cond
		((null client) " ")
		((listp client) (format " %s: " (car client)))
		(t (format " %s: " client)))
	      string)
      (or (bolp) (newline)))))

(defun dm-server-sentinel (proc msg)
  "The process sentinel for Emacs dm-server connections."
  ;; If this is a new client process, set the query-on-exit flag to nil
  ;; for this process (it isn't inherited from the dm-server process).
  (when (and (eq (process-status proc) 'open)
	     (process-query-on-exit-flag proc))
    (set-process-query-on-exit-flag proc nil))
  ;; Delete the associated connection file, if applicable.
  ;; Although there's no 100% guarantee that the file is owned by the
  ;; running Emacs instance, dm-server-start uses dm-server-running-p to check
  ;; for possible dm-servers before doing anything, so it *should* be ours.
  (and (process-contact proc :dm-server)
       (eq (process-status proc) 'closed)
       (ignore-errors (delete-file (process-get proc :dm-server-file))))
  (dm-server-log (format "Status changed to %s: %s" (process-status proc) msg) proc)
  (dm-server-delete-client proc))

(defun dm-server-select-display (display)
  ;; If the current frame is on `display' we're all set.
  ;; Similarly if we are unable to open frames on other displays, there's
  ;; nothing more we can do.
  (unless (or (not (fboundp 'make-frame-on-display))
              (equal (frame-parameter (selected-frame) 'display) display))
    ;; Otherwise, look for an existing frame there and select it.
    (dolist (frame (frame-list))
      (when (equal (frame-parameter frame 'display) display)
	(select-frame frame)))
    ;; If there's no frame on that display yet, create and select one.
    (unless (equal (frame-parameter (selected-frame) 'display) display)
      (let* ((buffer (generate-new-buffer " *dm-server-dummy*"))
             (frame (make-frame-on-display
                     display
                     ;; Make it display (and remember) some dummy buffer, so
                     ;; we can detect later if the frame is in use or not.
                     `((dm-server-dummy-buffer . ,buffer)
                       ;; This frame may be deleted later (see
                       ;; dm-server-unselect-display) so we want it to be as
                       ;; unobtrusive as possible.
                       (visibility . nil)))))
        (select-frame frame)
        (set-window-buffer (selected-window) buffer)
        frame))))

(defun dm-server-unselect-display (frame)
  (when (frame-live-p frame)
    ;; If the temporary frame is in use (displays something real), make it
    ;; visible.  If not (which can happen if the user's customizations call
    ;; pop-to-buffer etc.), delete it to avoid preserving the connection after
    ;; the last real frame is deleted.
    (if (and (eq (frame-first-window frame)
                 (next-window (frame-first-window frame) 'nomini))
             (eq (window-buffer (frame-first-window frame))
                 (frame-parameter frame 'dm-server-dummy-buffer)))
        ;; The temp frame still only shows one buffer, and that is the
        ;; internal temp buffer.
        (delete-frame frame)
      (set-frame-parameter frame 'visibility t))
    (kill-buffer (frame-parameter frame 'dm-server-dummy-buffer))
    (set-frame-parameter frame 'dm-server-dummy-buffer nil)))

(defun dm-server-handle-delete-frame (frame)
  "Delete the client connection when the emacsclient frame is deleted.
\(To be used from `delete-frame-functions'.)"
  (let ((proc (frame-parameter frame 'client)))
    (when (and (frame-live-p frame)
	       proc
	       ;; See if this is the last frame for this client.
	       (>= 1 (let ((frame-num 0))
		      (dolist (f (frame-list))
			(when (eq proc (frame-parameter f 'client))
			  (setq frame-num (1+ frame-num))))
		      frame-num)))
      (dm-server-log (format "dm-server-handle-delete-frame, frame %s" frame) proc)
      (dm-server-delete-client proc 'noframe)))) ; Let delete-frame delete the frame later.

(defun dm-server-handle-suspend-tty (terminal)
  "Notify the emacsclient process to suspend itself when its tty device is suspended."
  (dolist (proc (dm-server-clients-with 'terminal terminal))
    (dm-server-log (format "dm-server-handle-suspend-tty, terminal %s" terminal) proc)
    (condition-case err
	(dm-server-send-string proc "-suspend \n")
      (file-error                       ;The pipe/socket was closed.
       (ignore-errors (dm-server-delete-client proc))))))

(defun dm-server-unquote-arg (arg)
  "Remove &-quotation from ARG.
See `dm-server-quote-arg' and `dm-server-process-filter'."
  (replace-regexp-in-string
   "&." (lambda (s)
	  (case (aref s 1)
	    (?& "&")
	    (?- "-")
	    (?n "\n")
	    (t " ")))
   arg t t))

(defun dm-server-quote-arg (arg)
  "In ARG, insert a & before each &, each space, each newline, and -.
Change spaces to underscores, too, so that the return value never
contains a space.

See `dm-server-unquote-arg' and `dm-server-process-filter'."
  (replace-regexp-in-string
   "[-&\n ]" (lambda (s)
	       (case (aref s 0)
		 (?& "&&")
		 (?- "&-")
		 (?\n "&n")
		 (?\s "&_")))
   arg t t))

(defun dm-server-send-string (proc string)
  "A wrapper around `process-send-string' for logging."
  (dm-server-log (concat "Sent " string) proc)
  (process-send-string proc string))

(defun dm-server-ensure-safe-dir (dir)
  "Make sure DIR is a directory with no race-condition issues.
Creates the directory if necessary and makes sure:
- there's no symlink involved
- it's owned by us
- it's not readable/writable by anybody else."
  (setq dir (directory-file-name dir))
  (let ((attrs (file-attributes dir)))
    (unless attrs
      (letf (((default-file-modes) ?\700)) (make-directory dir t))
      (setq attrs (file-attributes dir)))
    ;; Check that it's safe for use.
    (unless (and (eq t (car attrs)) (eql (nth 2 attrs) (user-uid))
                 (or (eq system-type 'windows-nt)
                     (zerop (logand ?\077 (file-modes dir)))))
      (error "The directory %s is unsafe" dir))))

;;;###autoload
(defun dm-server-start (&optional leave-dead)
  "Allow this Emacs process to be a dm-server for client processes.
This starts a dm-server communications subprocess through which
client \"editors\" can send your editing commands to this Emacs
job.  To use the dm-server, set up the program `emacsclient' in the
Emacs distribution as your standard \"editor\".

Optional argument LEAVE-DEAD (interactively, a prefix arg) means just
kill any existing dm-server communications subprocess.

If a dm-server is already running, the dm-server is not started.
To force-start a dm-server, do \\[dm-server-force-delete] and then
\\[dm-server-start]."
  (interactive "P")
  (when (or (not dm-server-clients)
	    ;; Ask the user before deleting existing clients---except
	    ;; when we can't get user input, which may happen when
	    ;; doing emacsclient --eval "(kill-emacs)" in daemon mode.
	    (if (and (daemonp)
		     (null (cdr (frame-list)))
		     (eq (selected-frame) terminal-frame))
		leave-dead
	      (yes-or-no-p
	       "The current dm-server still has clients; delete them? ")))
    (let* ((dm-server-dir (if dm-server-use-tcp dm-server-auth-dir dm-server-socket-dir))
	   (dm-server-file (expand-file-name dm-server-name dm-server-dir)))
      (when dm-server-process
	;; kill it dead!
	(ignore-errors (delete-process dm-server-process)))
      ;; Delete the socket files made by previous dm-server invocations.
      (if (not (eq t (dm-server-running-p dm-server-name)))
	  ;; Remove any leftover socket or authentication file
	  (ignore-errors (delete-file dm-server-file))
	(setq dm-server-mode nil) ;; already set by the minor mode code
	(display-warning
	 'dm-server
	 (concat "Unable to start the Emacs dm-server.\n"
		 (format "There is an existing Emacs dm-server, named %S.\n"
			 dm-server-name)
		 "To start the dm-server in this Emacs process, stop the existing
dm-server or call `M-x dm-server-force-delete' to forcibly disconnect it.")
	 :warning)
	(setq leave-dead t))
      ;; If this Emacs already had a dm-server, clear out associated status.
      (while dm-server-clients
	(dm-server-delete-client (car dm-server-clients)))
      ;; Now any previous dm-server is properly stopped.
      (if leave-dead
	  (progn
	    (unless (eq t leave-dead) (dm-server-log (message "Dm-Server stopped")))
	    (setq dm-server-process nil))
	;; Make sure there is a safe directory in which to place the socket.
	(dm-server-ensure-safe-dir dm-server-dir)
	(when dm-server-process
	  (dm-server-log (message "Restarting dm-server")))
	(letf (((default-file-modes) ?\700))
	  (add-hook 'suspend-tty-functions 'dm-server-handle-suspend-tty)
	  (add-hook 'delete-frame-functions 'dm-server-handle-delete-frame)
	  (add-hook 'kill-buffer-query-functions 'dm-server-kill-buffer-query-function)
	  (add-hook 'kill-emacs-query-functions 'dm-server-kill-emacs-query-function)
	  (add-hook 'kill-emacs-hook (lambda () (dm-server-mode -1))) ;Cleanup upon exit.
	  (setq dm-server-process
		(apply #'make-network-process
		       :name dm-server-name
		       :server t
		       :noquery t
		       :sentinel 'dm-server-sentinel
		       :filter 'dm-server-process-filter
		       ;; We must receive file names without being decoded.
		       ;; Those are decoded by dm-server-process-filter according
		       ;; to file-name-coding-system.  Also don't get
		       ;; confused by CRs since we don't quote them.
		       :coding 'raw-text-unix
		       ;; The other args depend on the kind of socket used.
		       (if dm-server-use-tcp
			   (list :family nil
				 :service t
				 :host (or dm-server-host 'local)
				 :plist '(:authenticated nil))
			 (list :family 'local
			       :service (expand-file-name dm-server-name dm-server-socket-dir)
			       :plist '(:authenticated t)))))
	  (unless dm-server-process (error "Could not start dm-server process"))
	  (process-put dm-server-process :dm-server-file dm-server-file)
	  (when dm-server-use-tcp
	    (let ((auth-key
		   (loop
		      ;; The auth key is a 64-byte string of random chars in the
		      ;; range `!'..`~'.
		      for i below 64
		      collect (+ 33 (random 94)) into auth
		      finally return (concat auth))))
	      (process-put dm-server-process :auth-key auth-key)
	      (with-temp-file dm-server-file
		(set-buffer-multibyte nil)
		(setq buffer-file-coding-system 'no-conversion)
		(insert (format-network-address
			 (process-contact dm-server-process :local))
			" " (int-to-string (emacs-pid))
			"\n" auth-key)))))))))

;;;###autoload
(defun dm-server-force-delete (&optional name)
  "Unconditionally delete connection file for dm-server NAME.
If dm-server is running, it is first stopped.
NAME defaults to `dm-server-name'.  With argument, ask for NAME."
  (interactive
   (list (if current-prefix-arg
	     (read-string "Dm-Server name: " nil nil dm-server-name))))
  (when dm-server-mode (with-temp-message nil (dm-server-mode -1)))
  (let ((file (expand-file-name (or name dm-server-name)
				(if dm-server-use-tcp
				    dm-server-auth-dir
				  dm-server-socket-dir))))
    (condition-case nil
	(progn
	  (delete-file file)
	  (message "Connection file %S deleted" file))
      (file-error
       (message "No connection file %S" file)))))

(defun dm-server-running-p (&optional name)
  "Test whether dm-server NAME is running.

Return values:
  nil              the dm-server is definitely not running.
  t                the dm-server seems to be running.
  something else   we cannot determine whether it's running without using
                   commands which may have to wait for a long time."
  (unless name (setq name dm-server-name))
  (condition-case nil
      (if dm-server-use-tcp
	  (with-temp-buffer
	    (insert-file-contents-literally (expand-file-name name dm-server-auth-dir))
	    (or (and (looking-at "127\\.0\\.0\\.1:[0-9]+ \\([0-9]+\\)")
		     (assq 'comm
			   (process-attributes
			    (string-to-number (match-string 1))))
		     t)
		:other))
	(delete-process
	 (make-network-process
	  :name "dm-server-client-test" :family 'local :dm-server nil :noquery t
	  :service (expand-file-name name dm-server-socket-dir)))
	t)
    (file-error nil)))

;;;###autoload
(define-minor-mode dm-server-mode
  "Toggle Dm-Server mode.
With ARG, turn Dm-Server mode on if ARG is positive, off otherwise.
Dm-Server mode runs a process that accepts commands from the
`emacsclient' program.  See `dm-server-start' and Info node `Emacs dm-server'."
  :global t
  :group 'dm-server
  :version "22.1"
  ;; Fixme: Should this check for an existing dm-server socket and do
  ;; nothing if there is one (for multiple Emacs sessions)?
  (dm-server-start (not dm-server-mode)))

(defun dm-server-eval-and-print (expr proc)
  "Eval EXPR and send the result back to client PROC."
  (let ((v (eval (car (read-from-string expr)))))
    (when (and v proc)
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (pp v)
          (let ((text (buffer-substring-no-properties
                       (point-min) (point-max))))
            (dm-server-send-string
             proc (format "-print %s\n"
                          (dm-server-quote-arg text)))))))))

(defun dm-server-create-tty-frame (tty type proc)
  (unless tty
    (error "Invalid terminal device"))
  (unless type
    (error "Invalid terminal type"))
  (add-to-list 'frame-inherited-parameters 'client)
  (let ((frame
         (dm-server-with-environment (process-get proc 'env)
             '("LANG" "LC_CTYPE" "LC_ALL"
               ;; For tgetent(3); list according to ncurses(3).
               "BAUDRATE" "COLUMNS" "ESCDELAY" "HOME" "LINES"
               "NCURSES_ASSUMED_COLORS" "NCURSES_NO_PADDING"
               "NCURSES_NO_SETBUF" "TERM" "TERMCAP" "TERMINFO"
               "TERMINFO_DIRS" "TERMPATH"
               ;; rxvt wants these
               "COLORFGBG" "COLORTERM")
	     (make-frame `((window-system . nil)
			   (tty . ,tty)
			   (tty-type . ,type)
			   ;; Ignore nowait here; we always need to
			   ;; clean up opened ttys when the client dies.
			   (client . ,proc)
			   ;; This is a leftover from an earlier
			   ;; attempt at making it possible for process
			   ;; run in the dm-server process to use the
			   ;; environment of the client process.
			   ;; It has no effect now and to make it work
			   ;; we'd need to decide how to make
			   ;; process-environment interact with client
			   ;; envvars, and then to change the
			   ;; C functions `child_setup' and
			   ;; `getenv_internal' accordingly.
			   (environment . ,(process-get proc 'env)))))))

    ;; ttys don't use the `display' parameter, but callproc.c does to set
    ;; the DISPLAY environment on subprocesses.
    (set-frame-parameter frame 'display
                         (getenv-internal "DISPLAY" (process-get proc 'env)))
    (select-frame frame)
    (process-put proc 'frame frame)
    (process-put proc 'terminal (frame-terminal frame))

    ;; Display *scratch* by default.
    (switch-to-buffer (get-buffer-create "*scratch*") 'norecord)

    ;; Reply with our pid.
    (dm-server-send-string proc (concat "-emacs-pid "
                                     (number-to-string (emacs-pid)) "\n"))
    frame))

(defun dm-server-create-window-system-frame (display nowait proc)
  (add-to-list 'frame-inherited-parameters 'client)
  (if (not (fboundp 'make-frame-on-display))
      (progn
        ;; This emacs does not support X.
        (dm-server-log "Window system unsupported" proc)
        (dm-server-send-string proc "-window-system-unsupported \n")
        nil)
    ;; Flag frame as client-created, but use a dummy client.
    ;; This will prevent the frame from being deleted when
    ;; emacsclient quits while also preventing
    ;; `dm-server-save-buffers-kill-terminal' from unexpectedly
    ;; killing emacs on that frame.
    (let* ((params `((client . ,(if nowait 'nowait proc))
                     ;; This is a leftover, see above.
                     (environment . ,(process-get proc 'env))))
           (frame (make-frame-on-display
                   (or display
                       (frame-parameter nil 'display)
                       (getenv "DISPLAY")
                       (error "Please specify display"))
                   params)))
      (dm-server-log (format "%s created" frame) proc)
      (select-frame frame)
      (process-put proc 'frame frame)
      (process-put proc 'terminal (frame-terminal frame))

      ;; Display *scratch* by default.
      (switch-to-buffer (get-buffer-create "*scratch*") 'norecord)
      frame)))

(defun dm-server-goto-toplevel (proc)
  (condition-case nil
      ;; If we're running isearch, we must abort it to allow Emacs to
      ;; display the buffer and switch to it.
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (bound-and-true-p isearch-mode)
            (isearch-cancel))))
    ;; Signaled by isearch-cancel.
    (quit (message nil)))
  (when (> (recursion-depth) 0)
    ;; We're inside a minibuffer already, so if the emacs-client is trying
    ;; to open a frame on a new display, we might end up with an unusable
    ;; frame because input from that display will be blocked (until exiting
    ;; the minibuffer).  Better exit this minibuffer right away.
    ;; Similarly with recursive-edits such as the splash screen.
    (run-with-timer 0 nil (lexical-let ((proc proc))
			    (lambda () (dm-server-execute-continuation proc))))
    (top-level)))

;; We use various special properties on process objects:
;; - `env' stores the info about the environment of the emacsclient process.
;; - `continuation' is a no-arg function that we need to execute.  It contains
;;   commands we wanted to execute in some earlier invocation of the process
;;   filter but that we somehow were unable to process at that time
;;   (e.g. because we first need to throw to the toplevel).

(defun dm-server-execute-continuation (proc)
  (let ((continuation (process-get proc 'continuation)))
    (process-put proc 'continuation nil)
    (if continuation (ignore-errors (funcall continuation)))))

(defun* dm-server-process-filter (proc string)
  "Process a request from the dm-server to edit some files.
PROC is the dm-server process.  STRING consists of a sequence of
commands prefixed by a dash.  Some commands have arguments;
these are &-quoted and need to be decoded by `dm-server-unquote-arg'.
The filter parses and executes these commands.

To illustrate the protocol, here is an example command that
emacsclient sends to create a new X frame (note that the whole
sequence is sent on a single line):

	-env HOME=/home/lorentey
	-env DISPLAY=:0.0
	... lots of other -env commands
	-display :0.0
	-window-system

The following commands are accepted by the dm-server:

`-auth AUTH-STRING'
  Authenticate the client using the secret authentication string
  AUTH-STRING.

`-env NAME=VALUE'
  An environment variable on the client side.

`-dir DIRNAME'
  The current working directory of the client process.

`-current-frame'
  Forbid the creation of new frames.

`-nowait'
  Request that the next frame created should not be
  associated with this client.

`-display DISPLAY'
  Set the display name to open X frames on.

`-position LINE[:COLUMN]'
  Go to the given line and column number
  in the next file opened.

`-file FILENAME'
  Load the given file in the current frame.

`-eval EXPR'
  Evaluate EXPR as a Lisp expression and return the
  result in -print commands.

`-window-system'
  Open a new X frame.

`-tty DEVICENAME TYPE'
  Open a new tty frame at the client.

`-suspend'
  Suspend this tty frame.  The client sends this string in
  response to SIGTSTP and SIGTTOU.  The dm-server must cease all I/O
  on this tty until it gets a -resume command.

`-resume'
  Resume this tty frame.  The client sends this string when it
  gets the SIGCONT signal and it is the foreground process on its
  controlling tty.

`-ignore COMMENT'
  Do nothing, but put the comment in the dm-server log.
  Useful for debugging.


The following commands are accepted by the client:

`-emacs-pid PID'
  Describes the process id of the Emacs process;
  used to forward window change signals to it.

`-window-system-unsupported'
  Signals that the dm-server does not support creating X frames;
  the client must try again with a tty frame.

`-print STRING'
  Print STRING on stdout.  Used to send values
  returned by -eval.

`-error DESCRIPTION'
  Signal an error (but continue processing).

`-suspend'
  Suspend this terminal, i.e., stop the client process.
  Sent when the user presses C-z."
  (dm-server-log (concat "Received " string) proc)
  ;; First things first: let's check the authentication
  (unless (process-get proc :authenticated)
    (if (and (string-match "-auth \\([!-~]+\\)\n?" string)
	     (equal (match-string 1 string) (process-get proc :auth-key)))
	(progn
	  (setq string (substring string (match-end 0)))
	  (process-put proc :authenticated t)
	  (dm-server-log "Authentication successful" proc))
      (dm-server-log "Authentication failed" proc)
      (dm-server-send-string
       proc (concat "-error " (dm-server-quote-arg "Authentication failed")))
      (delete-process proc)
      ;; We return immediately
      (return-from dm-server-process-filter)))
  (let ((prev (process-get proc 'previous-string)))
    (when prev
      (setq string (concat prev string))
      (process-put proc 'previous-string nil)))
  (condition-case err
      (progn
	(dm-server-add-client proc)
	(if (not (string-match "\n" string))
            ;; Save for later any partial line that remains.
            (when (> (length string) 0)
              (process-put proc 'previous-string string))

          ;; In earlier versions of dm-server.el (where we used an `emacsdm-server'
          ;; process), there could be multiple lines.  Nowadays this is not
          ;; supported any more.
          (assert (eq (match-end 0) (length string)))
	  (let ((request (substring string 0 (match-beginning 0)))
		(coding-system (and default-enable-multibyte-characters
				    (or file-name-coding-system
					default-file-name-coding-system)))
		nowait ; t if emacsclient does not want to wait for us.
		frame ; The frame that was opened for the client (if any).
		display		     ; Open the frame on this display.
		dontkill       ; t if the client should not be killed.
		commands
		dir
		use-current-frame
		tty-name       ;nil, `window-system', or the tty name.
		tty-type             ;string.
		files
		filepos
		command-line-args-left
		arg)
	    ;; Remove this line from STRING.
	    (setq string (substring string (match-end 0)))
	    (setq command-line-args-left
		  (mapcar 'dm-server-unquote-arg (split-string request " " t)))
	    (while (setq arg (pop command-line-args-left))
		(cond
		 ;; -version CLIENT-VERSION: obsolete at birth.
		 ((and (equal "-version" arg) command-line-args-left)
		  (pop command-line-args-left))

		 ;; -nowait:  Emacsclient won't wait for a result.
		 ((equal "-nowait" arg) (setq nowait t))

		 ;; -current-frame:  Don't create frames.
		 ((equal "-current-frame" arg) (setq use-current-frame t))

		 ;; -display DISPLAY:
		 ;; Open X frames on the given display instead of the default.
		 ((and (equal "-display" arg) command-line-args-left)
		  (setq display (pop command-line-args-left))
                  (if (zerop (length display)) (setq display nil)))

		 ;; -window-system:  Open a new X frame.
		 ((equal "-window-system" arg)
                  (setq dontkill t)
                  (setq tty-name 'window-system))

		 ;; -resume:  Resume a suspended tty frame.
		 ((equal "-resume" arg)
		  (lexical-let ((terminal (process-get proc 'terminal)))
		    (setq dontkill t)
                    (push (lambda ()
                            (when (eq (terminal-live-p terminal) t)
                              (resume-tty terminal)))
                          commands)))

		 ;; -suspend:  Suspend the client's frame.  (In case we
		 ;; get out of sync, and a C-z sends a SIGTSTP to
		 ;; emacsclient.)
		 ((equal "-suspend" arg)
		  (lexical-let ((terminal (process-get proc 'terminal)))
		    (setq dontkill t)
                    (push (lambda ()
                            (when (eq (terminal-live-p terminal) t)
                              (suspend-tty terminal)))
                          commands)))

		 ;; -ignore COMMENT:  Noop; useful for debugging emacsclient.
		 ;; (The given comment appears in the dm-server log.)
		 ((and (equal "-ignore" arg) command-line-args-left
		  (setq dontkill t)
		  (pop command-line-args-left)))

		 ;; -tty DEVICE-NAME TYPE:  Open a new tty frame at the client.
		 ((and (equal "-tty" arg)
                       (cdr command-line-args-left))
                  (setq tty-name (pop command-line-args-left)
			tty-type (pop command-line-args-left)
			dontkill (or dontkill
				     (not use-current-frame))))

		 ;; -position LINE[:COLUMN]:  Set point to the given
		 ;;  position in the next file.
		 ((and (equal "-position" arg)
		       command-line-args-left
                       (string-match "\\+\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?"
                                     (car command-line-args-left)))
		  (setq arg (pop command-line-args-left))
		  (setq filepos
                        (cons (string-to-number (match-string 1 arg))
                              (string-to-number (or (match-string 2 arg) "")))))

		 ;; -file FILENAME:  Load the given file.
		 ((and (equal "-file" arg)
		       command-line-args-left)
		  (let ((file (pop command-line-args-left)))
		    (if coding-system
			(setq file (decode-coding-string file coding-system)))
                    (setq file (expand-file-name file dir))
		    (push (cons file filepos) files)
		    (dm-server-log (format "New file: %s %s"
                                        file (or filepos "")) proc))
		  (setq filepos nil))

		 ;; -eval EXPR:  Evaluate a Lisp expression.
		 ((and (equal "-eval" arg)
                       command-line-args-left)
		  (if use-current-frame
		      (setq use-current-frame 'always))
		  (lexical-let ((expr (pop command-line-args-left)))
		    (if coding-system
			(setq expr (decode-coding-string expr coding-system)))
                    (push (lambda () (dm-server-eval-and-print expr proc))
                          commands)
		    (setq filepos nil)))

		 ;; -env NAME=VALUE:  An environment variable.
		 ((and (equal "-env" arg) command-line-args-left)
		  (let ((var (pop command-line-args-left)))
		    ;; XXX Variables should be encoded as in getenv/setenv.
                    (process-put proc 'env
                                 (cons var (process-get proc 'env)))))

		 ;; -dir DIRNAME:  The cwd of the emacsclient process.
		 ((and (equal "-dir" arg) command-line-args-left)
		  (setq dir (pop command-line-args-left))
		  (if coding-system
		      (setq dir (decode-coding-string dir coding-system)))
		  (setq dir (command-line-normalize-file-name dir)))

		 ;; Unknown command.
		 (t (error "Unknown command: %s" arg))))

	    (setq frame
		  (cond
		   ((and use-current-frame
			 (or (eq use-current-frame 'always)
			     ;; We can't use the Emacs daemon's
			     ;; terminal frame.
			     (not (and (daemonp)
				       (null (cdr (frame-list)))
				       (eq (selected-frame)
					   terminal-frame)))))
		    (setq tty-name nil tty-type nil)
		    (if display (dm-server-select-display display)))
		   ((eq tty-name 'window-system)
		    (dm-server-create-window-system-frame display nowait proc))
		   ;; When resuming on a tty, tty-name is nil.
		   (tty-name
		    (dm-server-create-tty-frame tty-name tty-type proc))))

            (process-put
             proc 'continuation
             (lexical-let ((proc proc)
                           (files files)
                           (nowait nowait)
                           (commands commands)
                           (dontkill dontkill)
                           (frame frame)
                           (dir dir)
                           (tty-name tty-name))
               (lambda ()
                 (with-current-buffer (get-buffer-create dm-server-buffer)
                   ;; Use the same cwd as the emacsclient, if possible, so
                   ;; relative file names work correctly, even in `eval'.
                   (let ((default-directory
			  (if (and dir (file-directory-p dir))
			      dir default-directory)))
                     (dm-server-execute proc files nowait commands
                                     dontkill frame tty-name))))))

            (when (or frame files)
              (dm-server-goto-toplevel proc))

            (dm-server-execute-continuation proc))))
    ;; condition-case
    (error (dm-server-return-error proc err))))

(defun dm-server-execute (proc files nowait commands dontkill frame tty-name)
  ;; This is run from timers and process-filters, i.e. "asynchronously".
  ;; But w.r.t the user, this is not really asynchronous since the timer
  ;; is run after 0s and the process-filter is run in response to the
  ;; user running `emacsclient'.  So it is OK to override the
  ;; inhibit-quit flag, which is good since `commands' (as well as
  ;; find-file-noselect via the major-mode) can run arbitrary code,
  ;; including code that needs to wait.
  (with-local-quit
    (condition-case err
        (let* ((buffers
                (when files
                  (run-hooks 'pre-command-hook)
                  (prog1 (dm-server-visit-files files proc nowait)
                    (run-hooks 'post-command-hook)))))

          (mapc 'funcall (nreverse commands))

          ;; Delete the client if necessary.
          (cond
           (nowait
            ;; Client requested nowait; return immediately.
            (dm-server-log "Close nowait client" proc)
            (dm-server-delete-client proc))
           ((and (not dontkill) (null buffers))
            ;; This client is empty; get rid of it immediately.
            (dm-server-log "Close empty client" proc)
            (dm-server-delete-client proc)))
          (cond
           ((or isearch-mode (minibufferp))
            nil)
           ((and frame (null buffers))
            (message "%s" (substitute-command-keys
                           "When done with this frame, type \\[delete-frame]")))
           ((not (null buffers))
            (dm-server-switch-buffer (car buffers) nil (cdr (car files)))
            (run-hooks 'dm-server-switch-hook)
            (unless nowait
              (message "%s" (substitute-command-keys
                             "When done with a buffer, type \\[dm-server-edit]")))))
          (when (and frame (null tty-name))
            (dm-server-unselect-display frame)))
      (error (dm-server-return-error proc err)))))

(defun dm-server-return-error (proc err)
  (ignore-errors
    (dm-server-send-string
     proc (concat "-error " (dm-server-quote-arg
                             (error-message-string err))))
    (dm-server-log (error-message-string err) proc)
    (delete-process proc)))

(defun dm-server-goto-line-column (line-col)
  "Move point to the position indicated in LINE-COL.
LINE-COL should be a pair (LINE . COL)."
  (when line-col
    (goto-line (car line-col))
    (let ((column-number (cdr line-col)))
      (when (> column-number 0)
        (move-to-column (1- column-number))))))

(defun dm-server-visit-files (files proc &optional nowait)
  "Find FILES and return a list of buffers created.
FILES is an alist whose elements are (FILENAME . FILEPOS)
where FILEPOS can be nil or a pair (LINENUMBER . COLUMNNUMBER).
PROC is the client that requested this operation.
NOWAIT non-nil means this client is not waiting for the results,
so don't mark these buffers specially, just visit them normally."
  ;; Bind last-nonmenu-event to force use of keyboard, not mouse, for queries.
  (let ((last-nonmenu-event t) client-record)
    ;; Restore the current buffer afterward, but not using save-excursion,
    ;; because we don't want to save point in this buffer
    ;; if it happens to be one of those specified by the dm-server.
    (save-current-buffer
      (dolist (file files)
	;; If there is an existing buffer modified or the file is
	;; modified, revert it.  If there is an existing buffer with
	;; deleted file, offer to write it.
	(let* ((minibuffer-auto-raise (or dm-server-raise-frame
					  minibuffer-auto-raise))
	       (filen (car file))
	       (obuf (get-file-buffer filen)))
	  (add-to-history 'file-name-history filen)
	  (if (null obuf)
              (set-buffer (find-file-noselect filen))
            (set-buffer obuf)
            (cond ((file-exists-p filen)
                   (when (not (verify-visited-file-modtime obuf))
                     (revert-buffer t nil)))
                  (t
                   (when (y-or-n-p
                          (concat "File no longer exists: " filen
                                  ", write buffer to file? "))
                     (write-file filen))))
            (unless dm-server-buffer-clients
              (setq dm-server-existing-buffer t)))
          (dm-server-goto-line-column (cdr file))
          (run-hooks 'dm-server-visit-hook))
	(unless nowait
	  ;; When the buffer is killed, inform the clients.
	  (add-hook 'kill-buffer-hook 'dm-server-kill-buffer nil t)
	  (push proc dm-server-buffer-clients))
	(push (current-buffer) client-record)))
    (unless nowait
      (process-put proc 'buffers
                   (nconc (process-get proc 'buffers) client-record)))
    client-record))

(defun dm-server-buffer-done (buffer &optional for-killing)
  "Mark BUFFER as \"done\" for its client(s).
This buries the buffer, then returns a list of the form (NEXT-BUFFER KILLED).
NEXT-BUFFER is another dm-server buffer, as a suggestion for what to select next,
or nil.  KILLED is t if we killed BUFFER (typically, because it was visiting
a temp file).
FOR-KILLING if non-nil indicates that we are called from `kill-buffer'."
  (let ((next-buffer nil)
	(killed nil))
    (dolist (proc dm-server-clients)
      (let ((buffers (process-get proc 'buffers)))
	(or next-buffer
	    (setq next-buffer (nth 1 (memq buffer buffers))))
	(when buffers			; Ignore bufferless clients.
	  (setq buffers (delq buffer buffers))
	  ;; Delete all dead buffers from PROC.
	  (dolist (b buffers)
	    (and (bufferp b)
		 (not (buffer-live-p b))
		 (setq buffers (delq b buffers))))
	  (process-put proc 'buffers buffers)
	  ;; If client now has no pending buffers,
	  ;; tell it that it is done, and forget it entirely.
	  (unless buffers
	    (dm-server-log "Close" proc)
	    (if for-killing
		;; `dm-server-delete-client' might delete the client's
		;; frames, which might change the current buffer.  We
		;; don't want that (bug#640).
		(save-current-buffer
		  (dm-server-delete-client proc))
	      (dm-server-delete-client proc))))))
    (when (and (bufferp buffer) (buffer-name buffer))
      ;; We may or may not kill this buffer;
      ;; if we do, do not call dm-server-buffer-done recursively
      ;; from kill-buffer-hook.
      (let ((dm-server-kill-buffer-running t))
	(with-current-buffer buffer
	  (setq dm-server-buffer-clients nil)
	  (run-hooks 'dm-server-done-hook))
	;; Notice whether dm-server-done-hook killed the buffer.
	(if (null (buffer-name buffer))
	    (setq killed t)
	  ;; Don't bother killing or burying the buffer
	  ;; when we are called from kill-buffer.
	  (unless for-killing
	    (when (and (not killed)
		       dm-server-kill-new-buffers
		       (with-current-buffer buffer
			 (not dm-server-existing-buffer)))
	      (setq killed t)
	      (bury-buffer buffer)
	      (kill-buffer buffer))
	    (unless killed
	      (if (dm-server-temp-file-p buffer)
		  (progn
		    (kill-buffer buffer)
		    (setq killed t))
		(bury-buffer buffer)))))))
    (list next-buffer killed)))

(defun dm-server-temp-file-p (&optional buffer)
  "Return non-nil if BUFFER contains a file considered temporary.
These are files whose names suggest they are repeatedly
reused to pass information to another program.

The variable `dm-server-temp-file-regexp' controls which filenames
are considered temporary."
  (and (buffer-file-name buffer)
       (string-match-p dm-server-temp-file-regexp (buffer-file-name buffer))))

(defun dm-server-done ()
  "Offer to save current buffer, mark it as \"done\" for clients.
This kills or buries the buffer, then returns a list
of the form (NEXT-BUFFER KILLED).  NEXT-BUFFER is another dm-server buffer,
as a suggestion for what to select next, or nil.
KILLED is t if we killed BUFFER, which happens if it was created
specifically for the clients and did not exist before their request for it."
  (when dm-server-buffer-clients
    (if (dm-server-temp-file-p)
	;; For a temp file, save, and do make a non-numeric backup
	;; (unless make-backup-files is nil).
	(let ((version-control nil)
	      (buffer-backed-up nil))
	  (save-buffer))
      (when (and (buffer-modified-p)
		 buffer-file-name
		 (y-or-n-p (concat "Save file " buffer-file-name "? ")))
	(save-buffer)))
    (dm-server-buffer-done (current-buffer))))

;; Ask before killing a dm-server buffer.
;; It was suggested to release its client instead,
;; but I think that is dangerous--the client would proceed
;; using whatever is on disk in that file. -- rms.
(defun dm-server-kill-buffer-query-function ()
  "Ask before killing a dm-server buffer."
  (or (not dm-server-buffer-clients)
      (let ((res t))
	(dolist (proc dm-server-buffer-clients res)
          (when (and (memq proc dm-server-clients)
                     (eq (process-status proc) 'open))
            (setq res nil))))
      (yes-or-no-p (format "Buffer `%s' still has clients; kill it? "
			   (buffer-name (current-buffer))))))

(defun dm-server-kill-emacs-query-function ()
  "Ask before exiting Emacs if it has live clients."
  (or (not dm-server-clients)
      (let (live-client)
	(dolist (proc dm-server-clients live-client)
	  (when (memq t (mapcar 'buffer-live-p (process-get
						proc 'buffers)))
	    (setq live-client t))))
      (yes-or-no-p "This Emacs session has clients; exit anyway? ")))

(defvar dm-server-kill-buffer-running nil
  "Non-nil while `dm-server-kill-buffer' or `dm-server-buffer-done' is running.")

(defun dm-server-kill-buffer ()
  "Remove the current buffer from its clients' buffer list.
Designed to be added to `kill-buffer-hook'."
  ;; Prevent infinite recursion if user has made dm-server-done-hook
  ;; call kill-buffer.
  (or dm-server-kill-buffer-running
      (and dm-server-buffer-clients
	   (let ((dm-server-kill-buffer-running t))
	     (when dm-server-process
	       (dm-server-buffer-done (current-buffer) t))))))

(defun dm-server-edit (&optional arg)
  "Switch to next dm-server editing buffer; say \"Done\" for current buffer.
If a dm-server buffer is current, it is marked \"done\" and optionally saved.
The buffer is also killed if it did not exist before the clients asked for it.
When all of a client's buffers are marked as \"done\", the client is notified.

Temporary files such as MH <draft> files are always saved and backed up,
no questions asked.  (The variable `make-backup-files', if nil, still
inhibits a backup; you can set it locally in a particular buffer to
prevent a backup for it.)  The variable `dm-server-temp-file-regexp' controls
which filenames are considered temporary.

If invoked with a prefix argument, or if there is no dm-server process running,
starts dm-server process and that is all.  Invoked by \\[dm-server-edit]."
  (interactive "P")
  (cond
    ((or arg
         (not dm-server-process)
         (memq (process-status dm-server-process) '(signal exit)))
     (dm-server-mode 1))
    (dm-server-clients (apply 'dm-server-switch-buffer (dm-server-done)))
    (t (message "No dm-server editing buffers exist"))))

(defun dm-server-switch-buffer (&optional next-buffer killed-one filepos)
  "Switch to another buffer, preferably one that has a client.
Arg NEXT-BUFFER is a suggestion; if it is a live buffer, use it.

KILLED-ONE is t in a recursive call if we have already killed one
temp-file dm-server buffer.  This means we should avoid the final
\"switch to some other buffer\" since we've already effectively
done that.

FILEPOS specifies a new buffer position for NEXT-BUFFER, if we
visit NEXT-BUFFER in an existing window.  If non-nil, it should
be a cons cell (LINENUMBER . COLUMNNUMBER)."
  (if (null next-buffer)
      (progn
	(let ((rest dm-server-clients))
	  (while (and rest (not next-buffer))
	    (let ((proc (car rest)))
	      ;; Only look at frameless clients, or those in the selected
	      ;; frame.
	      (when (or (not (process-get proc 'frame))
			(eq (process-get proc 'frame) (selected-frame)))
		(setq next-buffer (car (process-get proc 'buffers))))
	      (setq rest (cdr rest)))))
	(and next-buffer (dm-server-switch-buffer next-buffer killed-one))
	(unless (or next-buffer killed-one (window-dedicated-p (selected-window)))
	  ;; (switch-to-buffer (other-buffer))
	  (message "No dm-server buffers remain to edit")))
    (if (not (buffer-live-p next-buffer))
	;; If NEXT-BUFFER is a dead buffer, remove the dm-server records for it
	;; and try the next surviving dm-server buffer.
	(apply 'dm-server-switch-buffer (dm-server-buffer-done next-buffer))
      ;; OK, we know next-buffer is live, let's display and select it.
      (if (functionp dm-server-window)
	  (funcall dm-server-window next-buffer)
	(let ((win (get-buffer-window next-buffer 0)))
	  (if (and win (not dm-server-window))
	      ;; The buffer is already displayed: just reuse the
	      ;; window.  If FILEPOS is non-nil, use it to replace the
	      ;; window's own value of point.
              (progn
                (select-window win)
                (set-buffer next-buffer)
		(when filepos
		  (dm-server-goto-line-column filepos)))
	    ;; Otherwise, let's find an appropriate window.
	    (cond ((window-live-p dm-server-window)
		   (select-window dm-server-window))
		  ((framep dm-server-window)
		   (unless (frame-live-p dm-server-window)
		     (setq dm-server-window (make-frame)))
		   (select-window (frame-selected-window dm-server-window))))
	    (when (window-minibuffer-p (selected-window))
	      (select-window (next-window nil 'nomini 0)))
	    ;; Move to a non-dedicated window, if we have one.
	    (when (window-dedicated-p (selected-window))
	      (select-window
	       (get-window-with-predicate
		(lambda (w)
		  (and (not (window-dedicated-p w))
		       (equal (frame-terminal (window-frame w))
			      (frame-terminal (selected-frame)))))
		'nomini 'visible (selected-window))))
	    (condition-case nil
		(switch-to-buffer next-buffer)
	      ;; After all the above, we might still have ended up with
	      ;; a minibuffer/dedicated-window (if there's no other).
	      (error (pop-to-buffer next-buffer)))))))
    (when dm-server-raise-frame
      (select-frame-set-input-focus (window-frame (selected-window))))))

;;;###autoload
(defun dm-server-save-buffers-kill-terminal (arg)
  ;; Called from save-buffers-kill-terminal in files.el.
  "Offer to save each buffer, then kill the current client.
With ARG non-nil, silently save all file-visiting buffers, then kill.

If emacsclient was started with a list of filenames to edit, then
only these files will be asked to be saved."
  (let ((proc (frame-parameter (selected-frame) 'client)))
    (cond ((eq proc 'nowait)
	   ;; Nowait frames have no client buffer list.
	   (if (cdr (frame-list))
	       (progn (save-some-buffers arg)
		      (delete-frame))
	     ;; If we're the last frame standing, kill Emacs.
	     (save-buffers-kill-emacs arg)))
	  ((processp proc)
	   (let ((buffers (process-get proc 'buffers)))
	     ;; If client is bufferless, emulate a normal Emacs exit
	     ;; and offer to save all buffers.  Otherwise, offer to
	     ;; save only the buffers belonging to the client.
	     (save-some-buffers
	      arg (if buffers
		      (lambda () (memq (current-buffer) buffers))
		    t))
	     (dm-server-delete-client proc)))
	  (t (error "Invalid client frame")))))

(define-key ctl-x-map "#" 'dm-server-edit)

(defun dm-server-unload-function ()
  "Unload the dm-server library."
  (dm-server-mode -1)
  (substitute-key-definition 'dm-server-edit nil ctl-x-map)
  (save-current-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (remove-hook 'kill-buffer-hook 'dm-server-kill-buffer t)))
  ;; continue standard unloading
  nil)


(provide 'dm-server)

;; arch-tag: 1f7ecb42-f00a-49f8-906d-61995d84c8d6
;;; dm-server.el ends here
