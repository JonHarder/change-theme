;;; theme-changer.el --- wrapper around changing themes for custom settings   -*- lexical-binding: t -*-

;; Author: Jon Harder <jonharder6@gmail.com>
;; Version: 1.0
;; Keywords: Theme

;;; Commentary:

;; This package provides a macro: make-theme, and two
;; functions, set-theme and next-theme.
;; This package came about through frustration at how
;; I needed to handle custimizations on a per theme basis
;; and switching between them.

;;; Code:

(defun theme-available-p (theme)
  (interactive)
  (if (memq theme (custom-available-themes))
      t
    nil))


(defvar theme-changer-themes (make-hash-table)
  "the hash table containing all the themes the user will create.
Themes are added in the form (puthash theme-name (lambda () ,@init-code))")


(defun theme-changer-available-themes ()
  (interactive)
  (let ((theme-names '()))
    (maphash (lambda (key _) (push (symbol-name key) theme-names)) theme-changer-themes)
    theme-names))


(defmacro theme-changer-make-theme (name &rest init-code)
  "creates a theme (really just a symbol for the name which is used by `load-theme' in
`theme-changer-set-theme', and a lambda for the initialization code when that theme
is selected.  It also adds that theme to `theme-changer-themes'."
  `(if (theme-available-p ,name)
       (progn (puthash ,name (lambda () ,@init-code) theme-changer-themes)
	      (message (concat "Added theme " (symbol-name ,name))))
     (message (concat "Theme '" (symbol-name ,name) "' not available for load-theme"))))


(defun theme-changer-set-theme (theme)
  "sets the theme to the THEME. If called interactively it will use
ido to select from the list of the themes added so far via `theme-changer-make-theme'."
  (interactive
   (list (ido-completing-read
	  "Select theme: "
	  (theme-changer-available-themes))))
  (when (stringp theme)
    (setq theme (intern theme)))
  (if (memq (symbol-name theme) (theme-changer-available-themes))
      (progn (dolist (tt custom-enabled-themes)
	       (disable-theme tt))
	     (load-theme theme)
	     (funcall (gethash theme theme-changer-themes)))
    (message (concat "Theme '" (symbol-name theme) "' not a recognized theme"))))


(provide 'theme-changer)
;;; theme-changer ends here
