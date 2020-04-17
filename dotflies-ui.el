;; (require 'dotflies)
(require 's)

;; TODO: Autoloads
(defvar dotflies-ui--buffer-name "*dotflies*")
(defvar dotflies-ui--default-state '(:row-selected 0
				     :col-selected 1))
(defconst dotflies-ui--cell-width 15)
;; phase 2, basic ui functions with updates
;; phased 3, style

;; Write-fns
(defun dotflies-ui--tabular-columns (columns)
  (cl-map 'vector
	  (lambda (col)
	    (list col dotflies-ui--cell-width nil))
	  columns))

(defun dotflies-ui--tabular-rows (rows)
  (-map-indexed (lambda (idx row)
		  (list idx (vconcat row)))
		rows))

(defun dotflies-ui--printer (id cols)
  (-let [str-cols (cl-map 'vector
			  (lambda (val)
			    (cond
			     ((null val) "")
			     ((or (symbolp val) (keywordp val))
			      (->> (symbol-name val)
				   (s-replace ":" "")
				   (s-replace "-" " ")
				   (s-titleize)))
			     (:else val)))
			  cols)]
    (tabulated-list-print-entry id str-cols)))

(defun dotflies-ui--hightlight-line (beg end)
  (interactive (list (point-at-bol) (point-at-eol)))
  (put-text-property beg end 'face 'highlight))

(defun dotflies-ui--unhighlight-line (beg end)
  (interactive (list (point-at-bol) (point-at-eol)))
  (remove-text-properties beg end '(face nil)))

(defun dotflies-ui--execute-sequence (fns)
  (seq-doseq (fn fns)
    (call-interactively fn)))

(defun dotflies-ui--move (&optional direction)
  (dotflies-ui--execute-sequence
   (list #'dotflies-ui--unhighlight-line
	 (cond
	  ((eq :up direction) #'previous-line)
	  (:else #'next-line))
	 #'dotflies-ui--hightlight-line)))

(defun dotflies-ui--show-row-info (current-row)
  (interactive (list (append (tabulated-list-get-entry) '())))
  (apply
   #'format
   "Name: %s\nLocation: %s\nBackup Dir: %s\nActive: %s"
   current-row))

(defun dotflies-ui--grid (columns rows)
  "Renders the grid given a list of `columns' and `rows'"
  (with-current-buffer (get-buffer-create dotflies-ui--buffer-name)
    ;; (popwin:display-buffer (current-buffer))
    ;; (Electric-pop-up-window (current-buffer) 50)
    (setq tabulated-list-padding 4)
    (setq tabulated-list-printer #'dotflies-ui--printer)
    (setq tabulated-list-format (dotflies-ui--tabular-columns columns))
    (setq tabulated-list-entries (dotflies-ui--tabular-rows rows))
    (tabulated-list-init-header)
    (tabulated-list-print))) 

(defvar dotflies-mode-map
  (-let [map (make-sparse-keymap)]
    (define-key map "k" (-partial #'dotflies-ui--move :up))
    (define-key map "j" #'dotflies-ui--move)
    ;; (define-key map "k" 'down)
    ;; (define-key map "l" 'down)
    map))

(define-derived-mode
  dotflies-mode
  tabulated-list-mode
  "Dotflies"
  "Major mode for dotfiles configurations"
  (font-lock-mode 1)
  (dotflies-ui--grid '("Config Name" "Location" "Backup Dir" "Active")
		     '(("emacs" "~/.emacs" "/emacs" :linked)
		       ("clojure" "~/.clojure/deps.edn" "/clj-deps" :not-linked)))
  )



;; Comment block
(defmacro comment (&rest body) nil)

(comment
 ;; (setq tabulated-list-format (vector 
 ;; 			     (list "name" dotflies-ui--cell-width nil)
 ;; 			     (list "value" dotflies-ui--cell-width nil)))
 ;; (setq tabulated-list-entries (list
 ;; 			      '(0 ["A" "B"])
 ;; 			      '(1 ["C" "D"])
 ;; 			      '(2 ["Testingianiginagiangtiahtiahti" "Test"])))
 

 (dotflies-ui--cell-padding "~/.clojure/deps.edn" 10 5 t)
 )

(comment

 (cl-map 'vector (lambda (a) a) '(1 2 3 4))
 
 (dotflies-ui--tabular-columns
  '("Config Name" "Location" "Backup Dir" "Active"))
 ;; (("Config Name" 12 nil) ("Location" 12 nil) ("Backup Dir" 12 nil) ("Active" 12 nil))
 (dotflies-ui--tabular-rows
  '(("emacs" "~/.emacs" "/emacs" :linked)
    ("clojure" "~/.clojure/deps.edn" "/clj-deps" :not-linked)))
 
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
 (insert (propertize "\n\ntesting" 'font-lock-keyword-face 'button))

 )

(provide 'dotflies-ui)

