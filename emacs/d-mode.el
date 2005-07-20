(defconst d-mode-version "$Revision: 1.5 $"
  "Revision number of d-mode.")

(make-face 'font-lock-function-name-face)
(make-face 'font-lock-type-face)

;;(set-face-foreground 'font-lock-function-name-face "yellow")
;;(set-face-bold-p 'font-lock-function-name-face nil)

;;(set-face-foreground 'font-lock-type-face "blue")
;;(set-face-bold-p 'font-lock-type-face nil)


(setq auto-mode-alist
      (append '(("\\.[dD]\\'" . d-mode))
	      auto-mode-alist))

(defvar d-mode-syntax-table nil
  "Syntax table used in d-mode.")

(defun d-mode-add-paren (pstart pend table)
  "Add PSTART and PEND as matching parens in TABLE."
  (modify-syntax-entry pstart (concat "(" (list pend)) table)
  (modify-syntax-entry pend (concat ")" (list pstart)) table))

(defun d-mode-add-parens (args table)
  "Add all in assoc list ARGS ((?\( . ?\)) ...) to TABLE."
  (mapc (lambda (pair) (d-mode-add-paren (car pair) (cdr pair) table))
	args))

(defun d-mode-add-simple (args table)
  "Add all (cdr ARGS) to TABLE with definition (car ARGS)."
  (let ((string (car args)))
    (mapc (lambda (arg) (modify-syntax-entry arg string table))
	(cdr args))))

(defconst d-mode-parens 
  '((?{ . ?})
    (?\[ . ?\]))
  "Assoc list of open-close paren pairs for d-mode.")

(defconst d-mode-spaces
  '(" " ?  ?\t ?\f ?\n ?\r )
  "List of spaces for d-mode.")

(defconst d-mode-comment-starts
  '("<" ?\( )
  "List of string starters for d-mode.")

(defconst d-mode-comment-starts-2
  '("<b" ?\< )
  "List of b string starters for d-mode.")

(defconst d-mode-comment-ends
  '(">" ?\) )
  "List of string enders for d-mode.")

(defconst d-mode-comment-ends-2
  '(">b" ?\> )
  "List of b string enders for d-mode.")

(defconst d-mode-prefixes
  '("'" ?~ ?/)
  "List of name prefixes for d-mode.")

(defconst d-mode-character-quotes
  '("\\" ?\\ )
  "List of character quote symbols for d-mode.")

(defconst d-mode-symbols
  '("w" ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?. ?+ ?- ?_)
  "List of symbols for d-mode")

(defconst d-mode-punctuation
  '("." ?\" ?\' ?*)
  "List of punctuation for d-mode")

(if (not d-mode-syntax-table)
    (setq d-mode-syntax-table 
	  (let ((table (make-syntax-table)))
	    (d-mode-add-parens d-mode-parens table)
	    (mapc (lambda (ss) (d-mode-add-simple ss table))
		  (list d-mode-spaces 
			d-mode-comment-starts
			d-mode-comment-ends
			d-mode-comment-starts-2
			d-mode-comment-ends-2
			d-mode-prefixes
			d-mode-character-quotes
			d-mode-symbols
			d-mode-punctuation))
	    table)))

(defun d-mode-magic-parens-key (arg)
  (interactive "*p")
  (let ((indent (>= (current-indentation) (current-column))))
    (self-insert-command (or arg 1))
    (if indent (d-mode-indent-line))))

(define-minor-mode d-mode-magic-parens "" nil "-p" 
  (mapcar (lambda (arg) (cons arg 'd-mode-magic-parens-key))
	  '("[" "]" "{" "}")))

(defun d-mode-magic-comment-key (arg)
  (interactive "*p")
  (let ((start-comment (and (not (d-mode-in-comment))
			    (save-excursion 
			      (mod (skip-chars-backward "\\") 2)))))
    (self-insert-command (or arg 1))
    (if start-comment (d-mode-indent-comment (- (point) 1)))))

(define-minor-mode d-mode-magic-comment "" nil "-c"
  '(("|" . d-mode-magic-comment-key)))

(defun d-mode-magic-newline-key (arg)
  "Puts in a newline, and if d-mode-magic-on is t, indents both before = 
and after."
  (interactive "*P")
  (setq arg (or arg 1))
  (while (> arg 0)
    (newline-and-indent)
    (setq arg (1- arg))))

(define-minor-mode d-mode-magic-newline "" nil "-n"
  '(("\C-m" . d-mode-magic-newline-key)))

(defun d-mode-in-comment ()
  (let ((state (parse-partial-sexp (line-beginning-position) (point))))
    (or (nth 3 state) (eq (char-after) ?\|))))

(defun d-mode-in-literal (&optional consider-comment-start)
  (interactive "P")
  (let ((state (parse-partial-sexp 1 (point))))
    (or (nth 3 state) (nth 4 state)
	(and consider-comment-start (eq (char-after) ?\|)))))

(define-minor-mode d-mode-magic-delete "" nil "-d"
  '(("\d" . d-mode-magic-delete-key)))
  
(defun d-mode-backspace-function (arg)
  "Redirect to backward-delete-char-untabify."
  (interactive "*P")
  (if (and delete-selection-mode mark-active)
      (delete-region (region-beginning) (region-end))
    (backward-delete-char-untabify (prefix-numeric-value arg))))

(defun d-mode-magic-delete-key (arg)
  "Delete over all space if d-mode-magic-delete-on."
  (interactive "*P")
  (if (or arg
	  (d-mode-in-literal t)
	  (and delete-selection-mode mark-active))
      (d-mode-backspace-function arg)
    (let ((here (point))
	  (leave-a-space (not (looking-at "[ \t\n\r\f]"))))
      (skip-chars-backward " \t\n\r\f")
      (let ((now (point)))
	(if (= now (1- here)) (setq leave-a-space nil))
	(if (< now here)
	    (progn
	      (goto-char here)
	      (delete-region now (if leave-a-space (1- here) here)))
	  (d-mode-backspace-function 1))))))

(defun d-mode-insert-tab (arg)
  (interactive "*p")
  (insert-char ?\  (* tab-width arg)))

(defvar d-mode-map nil
  "Key map to use in d-mode.")

(defun d-mode-setup-map ()
  (interactive)
  (setq d-mode-map
	(let ((mode-map (make-sparse-keymap)))
	  (define-key mode-map [(control c) (control n)]
	    'd-mode-magic-newline)
	  (define-key mode-map [(control c) (control p)] 
	    'd-mode-magic-parens)
	  (define-key mode-map [(control c) (control d)] 
	    'd-mode-magic-delete)
	  (define-key mode-map [(control c) (control c)]
	    'd-mode-magic-comment)
	  (define-key mode-map [S-iso-lefttab] 
	    'd-mode-insert-tab)
	  (define-key mode-map "\e\t"
	    'd-mode-insert-tab)
	  ;(define-key mode-map [(control tab)]
	  ;  'indent-regions)
	  mode-map)))

(if (not d-mode-map) (d-mode-setup-map))

(defconst d-mode-variable-name-regexp
  "[a-z_][a-z_0-9]\\{0,13\\}"
  "Regular expression for the simple name in d-mode.")

(defconst d-mode-variables-regexp
  (concat "\\s'?" d-mode-variable-name-regexp "\\>")
  "Regular expression for variables in d-mode.")

;; (defconst d-mode-comments-regexp
;;   "\\(^\\|[^\\\\]\\)\\(\\\\\\)\\{2\\}*\\(|.*$\\)"
;;   "Regular expression for comments in d-mode.")

(defconst d-mode-font-lock-keywords-1 ()
;;   (list
;;    (list d-mode-comments-regexp    3 'font-lock-string-face nil)
;;    )
  "Minimal set of font lock regexps for d-mode.")

(defconst d-mode-constants-regexp
  (concat "\\B\\*\\B\\|\\<\\(null\\|true\\|false"
	  "\\|[-+]?[0-9]+\\("
	  "\\(\\.[0-9]+\\)?\\(e[-+]?[0-9]+\\)?[sd]?\\|[sdbwl]?\\)\\)\\>")
  "Regular expression for a constant in d-mode.")

(defconst d-mode-marks-regexp
  "~?\\[\\|\\]\\|{\\|}"
  "Regular expression for paren markers.")

(defconst d-mode-font-lock-keywords-2
  (append d-mode-font-lock-keywords-1
   (list
    (list d-mode-constants-regexp 0 'font-lock-builtin-face)
    (list d-mode-variables-regexp 0 'font-lock-variable-name-face)
    (list d-mode-marks-regexp     0 'font-lock-type-face)))
  "Middle level font lock.")

(defconst d-mode-hrefs-regexp
  (concat "/" d-mode-variable-name-regexp "\\>")
  "Regular expression for local references in d-mode.")

(defconst d-mode-orefs-regexp 
  (concat "~" d-mode-variable-name-regexp "\\>")
  "Regular expression for foreign references in d-mode.")

(defconst d-mode-ops
  '("loadlib"
    "nextlib"
    "readboxfile"
    "writeboxfile"
	"startupdir"
    "hi"
    "def" 
    "exec" 
    "dict" 
    "dup"
    "begin"
    "name"
    "roll"
    "pop"
    "checkFPU"
    "if"
    "ifelse"
    "for"
    "repeat"
    "loop"
    "end"
    "not"
    "exch"
    "copy"
    "index"
    "clear"
    "count"
    "cleartomark"
    "counttomark"
    "abs"
    "neg"
    "sqrt"
    "cos"
    "acos"
    "sin"
    "asin"
    "tan"
    "atan"
    "exp"
    "ln"
    "lg"
    "floor"
    "ceil"
    "add"
    "sub"
    "mul"
    "div"
    "pwr"
    "mod"
    "thearc"
    "array"
    "length"
    "get"
    "put"
    "getinterval"
    "copy"
    "fax"
    "tosystem"
    "forall"
    "parcel"
    "anchorsearch"
    "search"
    "token"
    "list"
    "tobox"
    "frombox"
    "dict"
    "used"
    "known"
    "find"
    "merge"
    "systemdict"
    "userdict"
    "currentdict"
    "countdictstack"
    "dictstack"
    "eq"
    "ne"
    "ge"
    "gt"
    "le"
    "lt"
    "and"
    "or"
    "xor"
    "not"
    "bitshift"
    "start"
    "exit"
    "stop"
    "stopped"
    "halt"
    "continue"
    "error"
    "abort"
    "countexecstack"
    "execstack"
    "quit"
    "class"
    "type"
    "readonly"
    "active"
    "tilde"
    "parent"
    "mkread"
    "mkact"
    "mkpass"
    "ctype"
    "parcel"
    "text"
    "number"
    "bind"
    "vmstatus"
    "save"
    "capsave"
    "restore"
    "nextobject"
    "update"
    "toconsole"
    "findfiles"
    "readfile"
    "writefile"
    "gettime"
    "send"
    "console"
    "getmyname"
    "getmyport"
    "ramp"
    "tile"
    "extract"
    "interpolate"
    "extrema"
    "integrateOH"
    "integrateOHv"
    "integrateRS"
    "solvetridiag"
    "dilute"
    "dilute_add"
    "ran1"
    "solve_bandmat"
    "complexFFT"
    "realFFT"
    "decompLU"
    "backsubLU"
    "bandLU"
    "bandBS"
    "invertLU"
    "matmul"
    "mattranspose"
    "Xwindows"
    "Xdisplayname"
    "Xconnect"
    "Xdisconnect"
    "connect"
    "disconnect"
    "setconsole"
    "screensize"
    "makewindow"
    "deletewindow"
    "mapwindow"
    "resizewindow"
    "Xsync"
    "mapcolor"
    "drawline"
    "drawsymbols"
    "fillrectangle"
    "drawtext"
    "getsocket"
    "vmresize"
    "transcribe"
    "matvecmul"
    )
  "Operater names for d-mode.")

(make-face 'd-mode-oref-face "Face for ~ objects.")
(copy-face 'font-lock-function-name-face 'd-mode-oref-face)
(set-face-foreground 'd-mode-oref-face "red")
(defvar d-mode-oref-face 'd-mode-oref-face
  "Face for ~ objects.")

(defvar d-mode-font-lock-keywords-3 nil
  "Extreme set of font lock regexps for d-mode.")

(defun d-mode-font-lock-keywords-3 ()
  (setq d-mode-font-lock-keywords-3
	(append d-mode-font-lock-keywords-1
		(list
		 (list d-mode-hrefs-regexp     0 'font-lock-reference-face)
		 (list d-mode-orefs-regexp     0 'd-mode-oref-face)
		 (list d-mode-constants-regexp 0 'font-lock-builtin-face)
		 (list (regexp-opt d-mode-ops 'words) 
		       0 'font-lock-keyword-face))
		d-mode-font-lock-keywords-2)))

(if (not d-mode-font-lock-keywords-3)
    (d-mode-font-lock-keywords-3))

(defun d-mode-add-op (ops)
  (interactive "xList of keywords to add (\"x\" \"y\"):  ")
  (nconc d-mode-ops ops)
  (d-mode-font-lock-keywords-3)
  (when font-lock-mode
    (font-lock-mode)
    (font-lock-mode)))

(defvar d-mode-font-lock-keywords ()
  "Font lock keyword sym.")
(if (not d-mode-font-lock-keywords)
    (setq d-mode-font-lock-keywords d-mode-font-lock-keywords-3))

(defvar d-mode-hook nil
  "Hooks for d-mode.")

(defun d-mode-find-comment-column ()
  "Return the column of the last comment, not including the current one."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((len 1))
      (while (or (/= (mod len 2) 0) (d-mode-in-literal))
	(skip-chars-backward "^|")
	(backward-char)
	(setq len (save-excursion (skip-chars-backward "\\\\")))))
    (current-column)))

(defun d-mode-move-comment-to (column comment-start)
  "Move a comment so it lines up with the last possible comment."
  (let ((early-comment (and (< (current-column) column)
			    (= comment-start (point)))))
    (if  early-comment (forward-char))
    (save-excursion
      (goto-char comment-start)
      (let* ((comment-column (current-column))
	     (spaces (- column comment-column)))
	(cond ((> spaces 0)
	       (insert-char ?\  spaces))
	      ((< spaces 0)
	       (setq spaces (max spaces (skip-chars-backward " \t")))
	       (delete-char (- spaces))))))
    (if early-comment (backward-char))))

(defun d-mode-indent-non-comment ()
  "Indent if not inside a comment."
  (save-excursion	  
    (beginning-of-line)
    (when (not (d-mode-in-literal))
      (condition-case nil
	  (indent-line-to
	   (+
	    (save-excursion (up-list -1) (current-indentation))
	    (if (looking-at "[[:blank:]]*\\s)") 0 tab-width)))
	(error (indent-line-to 0)))))
  (if (> (current-indentation) (current-column))
      (move-to-column (current-indentation))))

(defun d-mode-indent-comment (comment-start)
  (d-mode-move-comment-to (d-mode-find-comment-column) comment-start))

(defun d-mode-indent-line ()
  "Indent current line as d code."
  (interactive)
  (let* ((state (parse-partial-sexp (line-beginning-position) (point)))
	 (comment-start
	  (let ((c0 (char-before (- (point) 1))))
	    (if (and (nth 3 state) c0 (/= c0 ?\n))
	      (nth 8 state)
	      (if (eq (char-after) ?\|) (point))))))
    (if (or (not comment-start) (not d-mode-magic-comment))
	(d-mode-indent-non-comment)
      (d-mode-indent-comment comment-start))))

(defconst d-mode-font-lock-syntactic-keywords
  (list
    (list "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\)\\{2\\}*\\(\\\\.\\)"
     1 "." t) ; make normal after slash
    (list 
     (concat "\\(?:^\\|[^\n\\]\\)\\(?:\\\\\\)\\{2\\}*"
	     "(\\(\\(?:\\\\.\\|[^\)]\\)*\\)")
	  1 "." t) ; make normal in a string (particularly <>)
    (list 
     (concat "^[^\(\n|]*\\(?:\\s<\\(?:\\\\.\\|\\S>\\)*\\s>[^\(\n|]*\\)*"
	     "\\(|\\)\\(.*\\)\\(\n\\)")
     '(1 "|" t) '(2 "." t) '(3 "|" t)) ; setup comments outside of strings
    ))

(defun d-mode-replace-cr ()
  (interactive)
  (save-excursion
    (let ((coding (coding-system-eol-type buffer-file-coding-system)))
      (print coding)
      (when (and 
	     (not (and
		   (vectorp coding)
		   (let (r) (dolist (i (mapcar 'symbol-name coding) r)
			      (setq r (or r (string-match "-unix$" i)))))))
	     (not (eq coding 0))
	     (y-or-n-p "Convert to unix line ends? "))
	(set-buffer-file-coding-system 'unix)))
    (beginning-of-buffer)
    (when (and (re-search-forward "\r+\n?" nil t)
	       (y-or-n-p "Eliminate carriage returns? "))
      (replace-match "\n")
      (while (re-search-forward "\r+\n?" nil nil)
	(replace-match "\n")))
      (when (and (buffer-modified-p)
		 (y-or-n-p "Carriage returns eliminated - Save? "))
	(save-buffer))))

(defun d-mode-shared ()
  ""
  (interactive)
  (set (make-local-variable 'font-lock-defaults) 
       '((d-mode-font-lock-keywords-1 
	  d-mode-font-lock-keywords-2
	  d-mode-font-lock-keywords-3)
	 nil t nil nil 
	 (font-lock-syntactic-keywords . d-mode-font-lock-syntactic-keywords)))

  (set (make-local-variable 'comment-start) "| ")
  (set (make-local-variable 'comment-end) " ")
  (set (make-local-variable 'comment-start-skip) "\\(\\s-*\\)|*\\s-*")

  (let ((comment-face font-lock-string-face)
	(string-face  font-lock-comment-face))
    (set (make-local-variable 'font-lock-comment-face)
	 comment-face)
    (set (make-local-variable 'font-lock-string-face)
	 string-face))
  (set-syntax-table d-mode-syntax-table)
)

(defun d-mode-comment-region (beg end &optional arg)
  (interactive "*r\nP")
  (save-excursion
    ;(goto-char end)
    ;(when (= (line-beginning-position) end)
    ;  (setq end (1- end)))

    (goto-char beg)
    (while (< (point) end)
      (insert "|")
      (while (search-forward "|" (line-end-position) t)
 	(replace-match "\\|" t t))
      (beginning-of-line 2))))

(defun d-mode-uncomment-region (beg end &optional arg)
  (interactive "*r\nP")
  (save-excursion
    ;(goto-char end)
    ;(when (= (line-beginning-position) end)
    ;  (setq end (1- end)))

    (goto-char beg)
    (while (< (point) end)
      (if (looking-at "\\([ \t\f]*\\)|")
	  (replace-match "\\1"))
      (while (search-forward "\\|" (line-end-position) t)
	(replace-match "|" t t))
      (beginning-of-line 2))))

;;;###autoload
(defun d-mode ()
  "Major mode for editing d code."
  (interactive)
  (kill-all-local-variables)
  
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  (set (make-local-variable 'indent-line-function)
       'd-mode-indent-line)
  (setq indent-tabs-mode nil)

  (set (make-local-variable 'comment-region)
       'd-mode-comment-region)

  (set (make-local-variable 'uncomment-region)
       'd-mode-uncomment-region)

  (use-local-map d-mode-map)

  (d-mode-shared)

  (setq major-mode 'd-mode)
  (setq mode-name "D Machine")

  (d-mode-replace-cr)
  (run-hooks 'd-mode-hook)
)


(provide 'd-mode)
