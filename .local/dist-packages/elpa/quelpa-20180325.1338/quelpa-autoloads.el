;;; quelpa-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "quelpa" "quelpa.el" (23294 21717 767645 38000))
;;; Generated autoloads from quelpa.el

(autoload 'quelpa-build-package "quelpa" "\
Create PACKAGE-NAME with VERSION.

The information in FILE-SPECS is used to gather files from
SOURCE-DIR.

The resulting package will be stored as a .el or .tar file in
TARGET-DIR, depending on whether there are multiple files.

Argument FILE-SPECS is a list of specs for source files, which
should be relative to SOURCE-DIR.  The specs can be wildcards,
and optionally specify different target paths.  They extended
syntax is currently only documented in the MELPA README.  You can
simply pass `quelpa-build-default-files-spec' in most cases.

Returns the archive entry for the package.

\(fn PACKAGE-NAME VERSION FILE-SPECS SOURCE-DIR TARGET-DIR)" nil nil)

(autoload 'quelpa-expand-recipe "quelpa" "\
Expand a given recipe name into full recipe.
If called interactively, let the user choose a recipe name and
insert the result into the current buffer.

\(fn RECIPE-NAME)" t nil)

(autoload 'quelpa-self-upgrade "quelpa" "\
Upgrade quelpa itself.
ARGS are additional options for the quelpa recipe.

\(fn &optional ARGS)" t nil)

(autoload 'quelpa-upgrade "quelpa" "\
Upgrade all packages found in `quelpa-cache'.
This provides an easy way to upgrade all the packages for which
the `quelpa' command has been run in the current Emacs session.

\(fn)" t nil)

(autoload 'quelpa "quelpa" "\
Build and install a package with quelpa.
ARG can be a package name (symbol) or a melpa recipe (list).
PLIST is a plist that may modify the build and/or fetch process.
If called interactively, `quelpa' will prompt for a MELPA package
to install.

When `quelpa' is called interactively with a prefix argument (e.g
C-u M-x quelpa) it will try to upgrade the given package even if
the global var `quelpa-upgrade-p' is set to nil.

\(fn ARG &rest PLIST)" t nil)

;;;***

;;;### (autoloads nil nil ("bootstrap.el" "quelpa-pkg.el") (23294
;;;;;;  21717 763645 154000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; quelpa-autoloads.el ends here
