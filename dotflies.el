;;; dotflies.el --- simple dotfile configuration     -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Daniel Pritchett

;; Author: Daniel Pritchett <daniel.alan.p@gmail.com>
;; Created: 1 April 2019
;; Version: 0.1.0
;; Keywords: dotfiles
;; Homepage: TBA

;; This file is not part of GNU Emacs.

;; This file is free software...

(require 's)
(require 'dash)

(defmacro comment (&rest body) nil)

(define-minor-mode dotflies-mode
  :init-value nil)

(defcustom dotflies/default-config-dir "~/dotfiles-test"
  "The directory that will be used to operate on configs
and will have your linked paths stored."
  :type 'string)

;; TODO - Maybe use custom DSL that is elisp parsable
(defcustom dotflies/default-config-file "dfconfig"
  "Name of the default config file."
  :type 'string)

(defun dotflies--join-paths (paths &optional base)
  (s-join "/" (cons (s-replace "~" (getenv "HOME") base) paths)))

(defun dotflies--config-relative-path (&rest paths)
  (dotflies--join-paths paths dotflies/default-config-dir))

(defun dotflies--home-relative-path (&rest paths)
  (dotflies--join-paths paths "~"))

(comment
 (dotflies--home-relative-path "projects" "something") ;; "/home/deepe/projects/something"
 (dotflies--config-relative-path "projects" "something") ;; "/home/deepe/dotfiles-test/projects/something"
 (dotflies--config-relative-path) ;;"/home/deepe/dotfiles-test"
 (dotflies--home-relative-path ;; "/home/deepe"
  ))

(defun dotflies--call-form (cmd-form &optional output)
  (seq-let [cmd-sym &rest args] cmd-form
    `(call-process ,(symbol-name cmd-sym) nil ,output t ,@args)))

(comment
 (macroexpand-all (dotflies--call-form '(lsa)))
 ;; (call-process "lsa" nil nil t)
 (macroexpand-all (dotflies--call-form '(lsa) t))
 ;; (call-process "lsa" nil t t)
 (macroexpand-all (dotflies--call-form '(lsa -a --test)))
 ;; (call-process "lsa" nil output t -a --test))

(defun dotflies--condition-form (cmd-forms)
  `(condition-case err
       (progn
	 ,@(seq-map #'dotflies--call-form cmd-forms)
	 ,@call-forms)
     (error err)))

(comment
 (macroexpand-all (dotflies--condition-form (list '(ps "aux"))))
 ;;(condition-case err (progn (call-process "ps" nil nil t "aux")) (error err))
 (with-temp-buffer
   (let ((status (call-process "ps" nil t t "-aux"))
	 (output-str (buffer-substring-no-properties (buffer-end -1) (buffer-end 1))))
     `(,status . ,output-str)))
 

(defun dotflies/config-data ()
  (with-temp-buffer
    (insert-file-contents (dotflies/config-path))
    (buffer-string)))

(defmacro dotflies/cmd (&rest cmd-forms)
  "Evaluate `cmd-form' and return result"
  (dotflies--condition-form cmd-forms))

(defmacro dotflies/cond-cmd (pred-cmd &rest cmd-forms)
  "given a `pred-cmd' execute `cmd-forms' if the result is 0"
  (declare (indent 1))
  `(if (= 0 (dotflies/cmd ,pred-cmd))
       (dotflies/cmd ,@cmd-forms)))

;;==== Flow 1 - Initiate dotflies
;; * With `dotflies/default-config-dir' set to the desired directory,
;; * Execute (dotflies/init-dotflies)'
;; + A config.yml file will be created
(defun dotflies/init-dotflies ()
  "Initializes dotfile directory and file"
  (let ((res-path (dotflies/home-relative-path dotflies/default-config-dir)))
    (dotflies/cmd 
     (mkdir res-path)
     (touch (dotflies/config-path)))))

;;==== Flow 2 - Create a symlink 
;; * With the config directory created and populated with
;;   at least one entry of the form:
;;   - alias: path/to/source
;;   With the alias being the sym link name and the path
;;   being the file to move to dotfiles,
;; * Execute (dotflies/run-config)
;; + The entry paths will be compied to the config directory
;;   and the symlink will exist in the original path.
(defun dotflies/cfg->cmd (cfg-entry)
  (seq-let [symlink-name source] cfg-entry
    (let ((path-to-symlink (dotflies--home-relative-path (symbol-name symlink-name)))
	  (path-to-source (dotflies/home-relative-path source)))
      (list
       `(test "-f" ,path-to-symlink)
       `(mv ,path-to-source ,path-to-symlink)
       `(ln "-s" ,path-to-symlink ,path-to-source)))))

(defun dotflies/cmd-form (commands)
  (seq-let [test-cmd mv-cmd ln-cmd] commands
    `(dotflies/cond-cmd
	 ,test-cmd
       ,mv-cmd
       ,ln-cmd)))

(defun dotflies/-load-config-commands ()
  (let* ((commands (seq-map #'dotflies/cfg->cmd (dotflies/config-data))))
    (seq-map #'dotflies/cmd-form commands)))

(defun dotflies/run-config ()
  (dotflies/cmd ,@(dotflies/-load-cofing-commands)))

;;=======Comments

(comment
 (with-temp-buffer
   ;;=> 1 doesn't exist
   (= 1 (call-process "test" nil t nil "-f" (dotflies--home-relative-path "something")))
   (call-process "test" nil t nil "-f" (dotflies--home-relative-path "something"))
   (call-process-shell-command "test -f someting" nil nil t)
   ;;=> 0 does exist
   (= 0 (call-process "test" nil t nil "-f" (dotflies--home-relative-path "testing")))
   (buffer-string))
 s-join "/" (cons dotflies/default-config-dir nil) '("test")
 )

(provide 'dotflies)
;;; dotflies.el ends here
