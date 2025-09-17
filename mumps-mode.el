(defvar mumps-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Standard ; comments
    (modify-syntax-entry ?\; "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    table))


(defun mumps--go-while (expr)
  (while (looking-at expr)
	(forward-char 1)))


(defun mumps-comment-line ()
  (interactive)
  (save-excursion
	(beginning-of-line)
	(mumps--go-while "[ \t.]")
	(pcase (char-after)
	  (?;
	   (delete-char 1)
	   (if (eq
			(char-after) ? )
		   (delete-char 1)))
	  (_
	   (insert "; ")))))


(defun mumps-comment-region ()
  "Comment or uncomment region, preserving dot indentation."
  (interactive)
  (if (use-region-p)
      (let ((end (copy-marker (region-end))))
        (deactivate-mark)
        (save-excursion
          (goto-char (region-beginning))
          (while (< (point) end)
            (mumps-comment-line)
            (forward-line))))
    (mumps-comment-line)))


(defconst mumps-keywords-lowercase-full
  '("break" "close" "continue" "do" "else" "elseif" "for" "goto"
	"halt" "hang" "if" "job" "kill" "lock" "merge" "new" "open" "print"
	"quit" "read" "set" "tcommit" "trestart" "trollback" "tstart" "use"
	"view" "while" "write" "xecute" "zallocate" "zbreak" "zdeallocate"
	"zhang" "zhorolog" "zinsert" "zkill" "zload" "znspace" "zprint"
	"zquit" "zremove" "zsave" "zsync" "zsystem" "ztcommit" "ztrap"
	"ztstart" "zuse" "zwithdraw" "zwrite" "zzdump"))


(defconst mumps-keywords-lowercase-abbreviation
  '("b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q"
	"r" "s" "tc" "tre" "tro" "ts" "u" "v" "w" "x" "za" "zb" "zd" "zh"
	"zho" "zi" "zk" "zl" "zn" "zp" "zq" "zr" "zs" "zsy" "ztc" "zt" "zts"
	"zu" "zw"))


(defconst mumps-keywords-uppercase-full
  (mapcar 'upcase mumps-keywords-lowercase-full))


(defconst mumps-keywords-uppercase-abbreviation
  (mapcar 'upcase mumps-keywords-lowercase-full))


(defun mumps-keywords ()
  (append mumps-keywords-lowercase-full
		  mumps-keywords-lowercase-abbreviation
		  mumps-keywords-uppercase-full
		  mumps-keywords-uppercase-abbreviation))


(defconst mumps-functions-lowercase-full
  '("ascii" "bit" "bitcount" "bitfind" "bitlogic" "case" "char" "data"
	"extract" "factor" "find" "fnumber" "get" "increment" "inumber"
	"isobject" "isvalidnum" "justify" "length" "list" "listbuild" "lb"
	"listdata" "listfind" "listfromstring" "listget" "listlength"
	"listnext" "listsame" "listtostring" "name" "next" "normalize"
	"number" "order" "piece" "qlength" "qsubscript" "query" "random"
	"reverse" "select" "sortbegin" "sortend" "stack" "system" "text"
	"translate" "view" "zabs" "zarccos" "zarcsin" "zarctan" "zband"
	"zbcount" "zbfind" "zbget" "zbit" "zzbitand" "zbitcount" "zbitfind"
	"zbitget" "zbitlen" "zbitnot" "zbitor" "zbitset" "zbitstr" "zbitxor"
	"zblen" "zbnot" "zboolean" "zbor" "zbset" "zbstr" "zbxor" "zconvert"
	"zcvt" "zcos" "zcot" "zcrc" "zcsc" "zcyc" "zdate" "zdateh"
	"zdatetime" "zdatetimeh" "zdevice" "zexp" "zf" "zhex" "zincrement"
	"zinfo" "zln" "zlog" "zmessage" "zname" "znext" "zobjclassmethod"
	"zobjproperty" "zorder" "zparse" "zpower" "zprevious" "zsearch"
	"zsec" "zseek" "zsin" "zsocket" "zsort" "zsqr" "zstrip" "ztan"
	"ztexp" "ztime" "ztimeh" "ztlog" "ztrnlmn" "zuci"))


(defconst mumps-functions-lowercase-abbreviation
  '("a" "c" "d" "e" "f" "fn" "g" "in" "i" "j" "l" "li" "lb" "ld" "lf"
	"lfs" "lg" "ll" "ls" "lts" "na" "n" "num" "o" "p" "q" "ql" "qs" "r"
	"s" "st" "t" "tr" "v" "zba" "zbc" "zbf" "zbg" "zbl" "zbn" "zb"
	"zbse" "zbst" "zbx" "zcvt" "zc" "zd" "zdh" "zdt" "zdth" "zdev" "zi"
	"zo" "zp" "zse" "zso" "zt" "zth" "ztl" "zu"))


(defconst mumps-functions-uppercase-full
  (mapcar 'upcase mumps-functions-lowercase-full))


(defconst mumps-functions-uppercase-abbreviation
  (mapcar 'upcase mumps-functions-lowercase-abbreviation))


(defun mumps-functions ()
  (append mumps-functions-lowercase-full
		  mumps-functions-lowercase-abbreviation
		  mumps-functions-uppercase-full
		  mumps-functions-uppercase-abbreviation))


(defun mumps-font-lock-keywords ()
  (list
   ;; comments
   '("///.*" . font-lock-comment-face)         ; /// comments
   '(";.*" . font-lock-comment-face)           ; ; comments
   '("^#;.*" . font-lock-comment-face)         ; #; comments at start of line
   '("\\s-#;.*" . font-lock-comment-face)      ; #; comments after whitespace

   ;; keywords
   `(,(concat "\\(?:^\\|\\s-\\)" (regexp-opt (mumps-keywords) t) "\\(?:\\s-\\|$\\)") 
     . font-lock-keyword-face)

   ;; functions
   `(,(concat "\\$" (regexp-opt (mumps-functions) t) "\\>") 
     . font-lock-builtin-face)))


;;;###autoload
(define-derived-mode mumps-mode prog-mode "mumps mode"
  "Mode for MUMPS programming language"
  :syntax-table mumps-mode-syntax-table
  (setq-local font-lock-defaults '(mumps-font-lock-keywords))
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\.*;+\\s-*")
  (setq-local comment-add 0))


;;;###autoload
(setq auto-mode-alist
	  (append '(("\\.m$" . mumps-mode)
				("\\.inc$" . mumps-mode)
				("\\.mac$" . mumps-mode))
			  auto-mode-alist))


(defun mumps--parse-function-call-components (string)
  """
returns (list err notation container member)

  notation:
   - object-script
   - mumps
"""
  (cond
   ;; ObjectScript syntax: class.method
   ((string-match "^\\([a-zA-Z0-9][a-zA-Z0-9.]*\\)\\.\\([a-zA-Z0-9]+\\)$" string)
    (let ((class (match-string 1 string))    ; container
          (method (match-string 2 string)))  ; member
      (list nil "object-script" class method)))
   
   ;; Traditional MUMPS syntax: $$tag^routine
   ((string-match "^\\$\\$\\([a-zA-Z0-9]+\\)\\^\\([a-zA-Z0-9]+\\)$" string)
    (let ((tag (match-string 2 string))      ; member
          (routine (match-string 3 string))) ; container
      (list nil "mumps" routine tag)))
   
   ;; Traditional MUMPS syntax: tag^routine (without $$ prefix)
   ((string-match "^\\([a-zA-Z0-9]+\\)\\^\\([a-zA-Z0-9]+\\)$" string)
    (let ((tag (match-string 1 string))      ; member
          (routine (match-string 2 string))) ; container
      (list nil "mumps" routine tag)))

   ;; Simple member only
   ((string-match "^\\$\\$\\([a-zA-Z0-9]+\\)$" string)
    (let ((member (match-string 1 string)))  ; member only, no container
      (list nil nil nil member)))
   
   ;; Simple member only (without $$ prefix)
   ((string-match "^\\([a-zA-Z0-9]+\\)$" string)
    (let ((member (match-string 1 string)))  ; member only, no container
      (list nil nil nil member)))
   
   ;; Invalid syntax that doesn't match any pattern
   (t (list nil nil nil nil))))


(defun mumps--extract-components-at-cursor ()
  (save-excursion
    (let* ((current-word (thing-at-point 'word))
           (line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           words
           target-word)
      
      ;; Split line by spaces, tabs, and other common delimiters
      (setq words (split-string line "[ \t,=]+" t))
      
      ;; Find which word contains the cursor
      (dolist (word words)
        (let ((word-start (string-match (regexp-quote word) line))
              (word-end))
          (when word-start
            (setq word-end (+ word-start (length word)))
            (when (and (>= (point) (+ (line-beginning-position) word-start))
                       (<= (point) (+ (line-beginning-position) word-end)))
              (setq target-word word)))))
      
      ;; Extract function name before parentheses
      (if target-word
          (progn
            (when (string-match "^'?\\([a-zA-Z0-9$^.]+\\)" target-word)
              (match-string 1 target-word)))
        ""))))


(defun mumps-go-to-def ()
  (interactive)
  (let* (filename)
	(seq-let (err notation container member) (mumps--parse-function-call-components (mumps--extract-components-at-cursor))
	  ;; set filename regarding a notation
	  (pcase notation
		("mumps"
		 (setq filename (concat container ".mac")))
		(_
		 (setq filename (buffer-file-name))))
	  (find-file filename)
	  (beginning-of-buffer)
	  (search-forward-regexp (concat "^" member)))))


(provide 'mumps-mode)
