;;; dotflies.el --- Major mode to evaluate dotfiles.

;; Copyright (C) 2019 Daniel Pritchett

;; Author: Daniel Pritchett <daniel.alan.p@gmail.com>
;; Created: 1 April 2019
;; Version: 0.1.0
;; Keywords: dotfiles
;; Homepage: TBA

;; This file is not part of GNU Emacs.

;; This file is free software...
(require 'f)
(require 's)
(require 'edn)
(require 'dash)

(defmacro comment (&rest body) nil)

(defcustom dotflies/default-config-dir "~/dotfiles-test"
  "The directory that will be used to operate on configs
and will have your linked paths stored."
  :type 'string)

;; TODO - Maybe use custom DSL that is elisp parsable
(defcustom dotflies/default-config-file "config.edn"
  "Name of the default config file."
  :type 'string)

(defun dotflies/env-path (path)
  (s-replace "~" (getenv "HOME") path))

(defun dotflies/-resolved-path (&rest paths)
  (dotflies/env-path
   (s-join "/" (cons dotflies/default-config-dir paths))))

(defun dotflies/config-path ()
  (dotflies/-resolved-path  
   dotflies/default-config-file))

(defun dotflies/-call-form (cmd-form)
  (seq-let [cmd-sym &rest args] cmd-form
    `(call-process ,(symbol-name cmd-sym) nil nil t ,@args)))

(defun dotflies/-condition-form (cmd-forms)
  (let ((call-forms (seq-map #'dotflies/-call-form cmd-forms)))
    `(condition-case err
	 (progn
	   ,@call-forms)
       (error err))))

(defun dotflies/config-data ()
  (edn-read
   (with-temp-buffer
     (insert-file-contents (dotflies/config-path))
     (buffer-string))))

(defmacro dotflies/cmd (&rest cmd-forms)
  "Evaluate `cmd-form' and return result"
  (dotflies/-condition-form cmd-forms))


;;==== Flow 1 - Initiate dotflies
;; * With `dotflies/default-config-dir' set to the desired directory,
;; * Execute (dotflies/init-dotflies)'
;; + A config.yml file will be created
(defun dotflies/init-dotflies ()
  "Initializes dotfile directory and file"
  (let ((res-path (dotflies/env-path dotflies/default-config-dir)))
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
    (let ((path-to-symlink (dotflies/-resolved-path (symbol-name symlink-name)))
	  (path-to-source (dotflies/env-path source)))
      (list
       `(mv ,path-to-source ,path-to-symlink)
       `(ln "-s" ,path-to-symlink ,path-to-source)))))

(defmacro dotflies/run-config ()
  (let ((commands (-flatten-n 1 (seq-map #'dotflies/cfg->cmd (dotflies/config-data)))))
    `(dotflies/cmd ,@commands)))

;;=======Comments

(comment
 (condition-case err
     (process-lines "ls" "-z")
   (error -1))
 (process-lines "ls" "-z"))

;;; dotflies.el ends here
