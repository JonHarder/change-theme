Theme Changer
=============

# Description
Theme changer is an emacs package wrapper around load-theme to ease the use of changing themes

This package grew out of my obsession with switching themes frequently and the frustrations
that came with it.  Namely:

* Themes overlap and cause artifacts when using plain `load-theme`
* Poor coupling between the theme to load and configuration code to run with that theme

## Usage:

The main way to use this package is via `theme-changer-make-theme` to create a theme
and `theme-changer-set-theme` to set a theme created by `theme-changer-make-theme`.

## Example:

```
(theme-changer-make-theme 'spacemacs-dark
                          (setq spacemacs-theme-org-height nil
			        spacemacs-theme-org-highlight t))

(theme-changer-set-theme 'spacemacs-dark)
```

`theme-changer-set-theme` is also an interactive function, using `ido-comleting-read`
to allow you to select from any theme created with `theme-changer-make-theme`.

# TODO:

* impliment a `theme-changer-next-theme`
* provide themes for those that come included with stock emacs