;;; test-dotflies.el --- Helpers for evil-collection-test.el -*- lexical-binding: t -*-
(require 'ert)

(require 'f)
(let ((evil-collection-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path evil-collection-dir))

;; (require 'evil-collection)
;;; test-dotflies.el ends here
