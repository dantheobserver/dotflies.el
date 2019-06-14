(require 'dotflies)
(require 's)

;; TODO: Autoloads

(defvar dotflies-ui-buffer-name "*dotflies-explorer*")

(defun spaced-str (str width margin)
  "Reurn a `string' spaced properly with `width' and added `margin'"
  (let* ((str-count (length str))
	 (real-margin (+ margin
			 (- width str-count))))
    (s-concat str
	      (s-repeat real-margin " "))))


(defun dotflies-ui--entry-str (str entry)
  (let ((alias-col-width 10)
	(margin 5))
    (seq-let [alias-sym location] entry
      (s-concat str
		"* "
		(spaced-str (symbol-name alias-sym)
			    alias-col-width
			    margin)
		location
		"\n"))))

(defun dotflies-ui-display-config (config-file)
  (interactive)
  (let* ((buffer (get-buffer-create dotflies-ui-buffer-name))
	 (dummy-config '((testing "~/examples/testfile_1")
			 (other "~/examples/test-file-2")))
	 (entry-rows (seq-reduce #'dotflies-ui--entry-str dummy-config "")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (insert entry-rows)
      (read-only-mode)
      (Electric-pop-up-window buffer 12))))

(comment
 (let* ((buffer-name "*testing*")
	(buffer (get-buffer-create buffer-name)))
   (with-current-buffer buffer
     (insert "Testing 123")
     (Electric-pop-up-window buffer)))

 (seq-reduce #'+ '(1 2 3) 50)
 (dolist a b)
 (Electric-pop-up-window "*testing")

 (spaced-str "hello11111" 10 10)

 (dotflies-ui-display-config nil)
 ;; table macro 
 (buffer-table
  table-source
  ([header-name-a 30] header-name-b)) ;;binds to each element in the table source rows optional margin
 )
