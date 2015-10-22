;;; change-theme.el --- wrapper around load-theme for custom settings   -*- lexical-binding: t -*-

;; Author: Jon Harder <jonharder6@gmail.com>
;; Version: 1.0
;; Keywords: Theme
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; This package provides a macro: make-theme, and two
;; functions, set-theme and next-theme.
;; This package came about through frustration at how
;; I needed to handle custimizations on a per theme basis
;; and switching between them.

;;; Code:

(defun change-theme-theme-available-p (theme)
  (interactive)
  (if (memq theme (custom-available-themes))
      t
    nil))


(defvar change-theme-themes (make-hash-table)
  "the hash table containing all the themes the user will create.
Themes are added in the form (puthash theme-name (lambda () ,@init-code))")


(defun change-theme-available-themes ()
  (interactive)
  (let ((theme-names '()))
    (maphash (lambda (key _) (push (symbol-name key) theme-names)) change-theme-themes)
    theme-names))


(defmacro change-theme-make-theme (name &rest init-code)
  "creates a theme (really just a symbol for the name which is used by `load-theme' in
`change-theme-set-theme', and a lambda for the initialization code when that theme
is selected.  It also adds that theme to `change-theme-themes'."
  `(if (change-theme-theme-available-p ,name)
       (progn (puthash ,name (lambda () ,@init-code) change-theme-themes)
	      (message (concat "Added theme " (symbol-name ,name))))
     (message (concat "Theme '" (symbol-name ,name) "' not available for load-theme"))))


(defun change-theme-set-theme (theme)
  "sets the theme to the THEME. If called interactively it will use
ido to select from the list of the themes added so far via `change-theme-make-theme'."
  (interactive
   (list (ido-completing-read
	  "Select theme: "
	  (change-theme-available-themes))))
  (when (stringp theme)
    (setq theme (intern theme)))
  (if (memq (symbol-name theme) (change-theme-available-themes))
      (progn (dolist (tt custom-enabled-themes)
	       (disable-theme tt))
	     (load-theme theme t)
	     (funcall (gethash theme change-theme-themes)))
    (message (concat "Theme '" (symbol-name theme) "' not a recognized theme"))))


(provide 'change-theme)
;;; change-theme.el ends here
