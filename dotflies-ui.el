(require 'dotflies)
(require 's)

;; TODO: Autoloads
(defvar dotflies-ui-buffer-name "*dotflies-explorer*")
(defvar dotflies-ui-state '((cfg-line . 0)))

;; Faces
(defface dotflies-ui-title
  '((t :background "DarkGrey"))
  "Face for title text")

(defun dotflies-ui--ellipsize (str max-width)
  (let ((str-len (length str)))
    (if (< max-width str-len)
	(let ((substr (substring str 0 (- max-width 3))))
	  (s-concat substr "..."))
      str)))

(defun dotflies-ui--cell-str (str width &optional margin truncate)
  "Reurn a `string' spaced properly with `width' and added `margin'"
  (let* ((str-count (length str))
	 (real-margin (+ (or margin 0)
			 (- width str-count)))
	 (final-str (if truncate
			(dotflies-ui--ellipsize str width)
		      str)))
    (s-concat final-str
	      (s-repeat real-margin " "))))

(defun dotflies-ui--entry-str (str entry)
  (let ((alias-col-width 10)
	(loc-col-width 2)
	(margin 5))
    (seq-let [alias-sym location] entry
      (s-concat str
		"* "
		(dotflies-ui--cell-str (symbol-name alias-sym)
				       alias-col-width
				       margin)
		(dotflies-ui--cell-str location loc-col-width nil t)
		"\n"))))

(defun dotflies-ui-display-config ()
  (interactive)
  (let* ((buffer (get-buffer-create dotflies-ui-buffer-name))
	 (dummy-config '((testing "~/examples/testfile_1")
			 (other "~/examples/test-file-2")))
	 (entry-rows (seq-reduce #'dotflies-ui--entry-str dummy-config "")))
    (with-current-buffer buffer
      (setq-local font-lock-mode nil)
      (read-only-mode -1)
      (insert (propertize "Dotflies Configs" 'face 'italic) "\n")
      (insert entry-rows)
      (read-only-mode)
      (Electric-pop-up-window buffer 12)
      (read-only-mode 1))))

;; Comment block
(defmacro comment (&rest body) nil)
(comment
 (dotflies-ui-display-config nil) 
 (let* ((buffer-name "*testing*")
	(buffer (get-buffer-create buffer-name)))
   (with-current-buffer buffer
     (setq-local font-lock-mode nil)
     (insert "Testing 123")
     (Electric-pop-up-window buffer)))

 (seq-reduce #'+ '(1 2 3) 50)
 (dolist a b)
 (Electric-pop-up-window "*testing")

 (dotflies-ui--cell-str "hello11111" 10 10)

 ;; table macro 
 (buffer-table
  table-source
  ([header-name-a 30] header-name-b)) ;;binds to each element in the table source rows optional margin

 ;;propertizing
 (insert (propertize "\n\ntesting" 'face 'button))

 )
