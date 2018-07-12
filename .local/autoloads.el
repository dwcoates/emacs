;;; autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../doom-core/autoload/buffers" "../doom-core/autoload/buffers.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/buffers.el

(defvar doom-real-buffer-functions 'nil "\
A list of predicate functions run to determine if a buffer is real. These
functions are iterated over with one argument, the buffer in question. If any
function returns non-nil, the procession stops and the buffer is qualified as
real.")

(defvar-local doom-real-buffer-p nil "\
If non-nil, this buffer should be considered real no matter what.")

(defvar doom-fallback-buffer "*scratch*" "\
The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")

(autoload 'doom-fallback-buffer "../doom-core/autoload/buffers" "\
Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer.

\(fn)" nil nil)

(defalias 'doom-buffer-list #'buffer-list)

(autoload 'doom-project-buffer-list "../doom-core/autoload/buffers" "\
Return a list of buffers belonging to the current project.

If no project is active, return all buffers.

\(fn)" nil nil)

(autoload 'doom-real-buffer-list "../doom-core/autoload/buffers" "\
Return a list of buffers that satify `doom-real-buffer-p'.

\(fn &optional BUFFER-LIST)" nil nil)

(autoload 'doom-real-buffer-p "../doom-core/autoload/buffers" "\
Returns t if BUFFER-OR-NAME is a 'real' buffer. The complete criteria for a
real buffer is:

  1. The buffer-local value of `doom-real-buffer-p' (variable) is non-nil OR
  2. Any function in `doom-real-buffer-functions' must return non-nil when
     passed this buffer OR
  3. The current buffer:
     a) has a `buffer-file-name' defined AND
     b) is not in a popup window (see `doom-popup-p') AND
     c) is not a special buffer (its name isn't something like *Help*)

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested.

\(fn &optional BUFFER-OR-NAME)" nil nil)

(autoload 'doom-buffers-in-mode "../doom-core/autoload/buffers" "\
Return a list of buffers whose `major-mode' is `eq' to MODE(S).

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'.

\(fn MODES &optional BUFFER-LIST DERIVED-P)" nil nil)

(autoload 'doom-visible-windows "../doom-core/autoload/buffers" "\
Return a list of the visible, non-popup windows.

\(fn &optional WINDOW-LIST)" nil nil)

(autoload 'doom-visible-buffers "../doom-core/autoload/buffers" "\
Return a list of visible buffers (i.e. not buried).

\(fn &optional BUFFER-LIST)" nil nil)

(autoload 'doom-buried-buffers "../doom-core/autoload/buffers" "\
Get a list of buffers that are buried.

\(fn &optional BUFFER-LIST)" nil nil)

(autoload 'doom-matching-buffers "../doom-core/autoload/buffers" "\
Get a list of all buffers that match the regex PATTERN.

\(fn PATTERN &optional BUFFER-LIST)" nil nil)

(autoload 'doom-set-buffer-real "../doom-core/autoload/buffers" "\
Forcibly mark BUFFER as FLAG (non-nil = real).

\(fn BUFFER FLAG)" nil nil)

(autoload 'doom-kill-buffer "../doom-core/autoload/buffers" "\
Kill BUFFER (defaults to current buffer), but make sure we land on a real
buffer. Bury the buffer if the buffer is present in another window.

Will prompt to save unsaved buffers when attempting to kill them, unless
DONT-SAVE is non-nil.

See `doom-real-buffer-p' for what 'real' means.

\(fn &optional BUFFER DONT-SAVE)" nil nil)

(autoload 'doom-kill-buffer-and-windows "../doom-core/autoload/buffers" "\
Kill the buffer and delete all the windows it's displayed in.

\(fn BUFFER)" nil nil)

(autoload 'doom-kill-matching-buffers "../doom-core/autoload/buffers" "\
Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers.

\(fn PATTERN &optional BUFFER-LIST)" nil nil)

(autoload 'doom/kill-this-buffer "../doom-core/autoload/buffers" "\
Use `doom-kill-buffer' on the current buffer.

\(fn &optional INTERACTIVE-P)" t nil)

(autoload 'doom/kill-this-buffer-in-all-windows "../doom-core/autoload/buffers" "\
Kill BUFFER globally and ensure all windows previously showing this buffer
have switched to a real buffer.

If DONT-SAVE, don't prompt to save modified buffers (discarding their changes).

\(fn BUFFER &optional DONT-SAVE)" t nil)

(autoload 'doom/kill-all-buffers "../doom-core/autoload/buffers" "\
Kill all buffers and closes their windows.

If PROJECT-P (universal argument), kill only buffers that belong to the current
project.

\(fn &optional PROJECT-P)" t nil)

(autoload 'doom/kill-other-buffers "../doom-core/autoload/buffers" "\
Kill all other buffers (besides the current one).

If PROJECT-P (universal argument), kill only buffers that belong to the current
project.

\(fn &optional PROJECT-P)" t nil)

(autoload 'doom/kill-matching-buffers "../doom-core/autoload/buffers" "\
Kill buffers that match PATTERN in BUFFER-LIST.

If PROJECT-P (universal argument), only kill matching buffers in the current
project.

\(fn PATTERN &optional PROJECT-P)" t nil)

(autoload 'doom/cleanup-session "../doom-core/autoload/buffers" "\
Clean up buried buries and orphaned processes in the current workspace. If
ALL-P (universal argument), clean them up globally.

\(fn &optional ALL-P)" t nil)

(autoload 'doom/cleanup-processes "../doom-core/autoload/buffers" "\
Kill all processes that have no visible associated buffers. Return number of
processes killed.

\(fn)" t nil)

(autoload 'doom/next-buffer "../doom-core/autoload/buffers" "\
Switch to the next real buffer, skipping non-real buffers. See
`doom-real-buffer-p' for what 'real' means.

\(fn)" t nil)

(autoload 'doom/previous-buffer "../doom-core/autoload/buffers" "\
Switch to the previous real buffer, skipping non-real buffers. See
`doom-real-buffer-p' for what 'real' means.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/debug" "../doom-core/autoload/debug.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/debug.el

(autoload 'doom/what-face "../doom-core/autoload/debug" "\
Shows all faces and overlay faces at point.

Interactively prints the list to the echo area. Noninteractively, returns a list
whose car is the list of faces and cadr is the list of overlay faces.

\(fn &optional POS)" t nil)

(autoload 'doom-active-minor-modes "../doom-core/autoload/debug" "\
Get a list of active minor-mode symbols.

\(fn)" nil nil)

(autoload 'doom/what-minor-mode "../doom-core/autoload/debug" "\
Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not.

\(fn MODE)" t nil)

(autoload 'doom/am-i-secure "../doom-core/autoload/debug" "\
Test to see if your root certificates are securely configured in emacs.

\(fn)" t nil)

(function-put 'doom/am-i-secure 'interactive-only 't)

(autoload 'doom/toggle-profiler "../doom-core/autoload/debug" "\


\(fn)" t nil)

(autoload 'doom/info "../doom-core/autoload/debug" "\
Collects information about this session of Doom Emacs and copies it to the
clipboard. Helpful when filing bug reports!

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/editor" "../doom-core/autoload/editor.el"
;;;;;;  (23322 53895 496622 129000))
;;; Generated autoloads from ../doom-core/autoload/editor.el

(autoload 'doom/sudo-find-file "../doom-core/autoload/editor" "\
Open FILE as root.

\(fn FILE)" t nil)

(autoload 'doom/sudo-this-file "../doom-core/autoload/editor" "\
Open the current file as root.

\(fn)" t nil)

(autoload 'doom/backward-to-bol-or-indent "../doom-core/autoload/editor" "\
Move back to the current line's indentation. If already there, move to the
beginning of the line instead. If at bol, do nothing.

\(fn)" t nil)

(autoload 'doom/forward-to-last-non-comment-or-eol "../doom-core/autoload/editor" "\
Move forward to the last non-blank character in the line, ignoring comments
and trailing whitespace. If already there, move to the real end of the line.
If already there, do nothing.

\(fn)" t nil)

(autoload 'doom/dumb-indent "../doom-core/autoload/editor" "\
Inserts a tab character (or spaces x tab-width).

\(fn)" t nil)

(autoload 'doom/dumb-dedent "../doom-core/autoload/editor" "\
Dedents the current line.

\(fn)" t nil)

(autoload 'doom/backward-kill-to-bol-and-indent "../doom-core/autoload/editor" "\
Kill line to the first non-blank character. If invoked again
afterwards, kill line to column 1.

\(fn)" t nil)

(autoload 'doom/backward-delete-whitespace-to-column "../doom-core/autoload/editor" "\
Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible.

\(fn)" t nil)

(autoload 'doom/inflate-space-maybe "../doom-core/autoload/editor" "\
Checks if point is surrounded by {} [] () delimiters and adds a
space on either side of the point if so.

\(fn)" t nil)

(autoload 'doom/deflate-space-maybe "../doom-core/autoload/editor" "\
Checks if point is surrounded by {} [] () delimiters, and deletes
spaces on either side of the point if so. Resorts to
`doom/backward-delete-whitespace-to-column' otherwise.

\(fn)" t nil)

(autoload 'doom/newline-and-indent "../doom-core/autoload/editor" "\
Inserts a newline and possibly indents it. Also continues comments if
executed from a commented line; handling special cases for certain languages
with weak native support.

\(fn)" t nil)

(autoload 'doom/retab "../doom-core/autoload/editor" "\
Changes all tabs to spaces or spaces to tabs, so that indentation is
consistent throughout a selected region, depending on `indent-tab-mode'.

\(fn &optional BEG END)" t nil)

(autoload 'doom/narrow-buffer "../doom-core/autoload/editor" "\
Restrict editing in this buffer to the current region, indirectly. With CLONE-P,
clone the buffer and hard-narrow the selection. If mark isn't active, then widen
the buffer (if narrowed).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/

\(fn BEG END &optional CLONE-P)" t nil)

(autoload 'doom|enable-delete-trailing-whitespace "../doom-core/autoload/editor" "\
Attaches `delete-trailing-whitespace' to a buffer-local `before-save-hook'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/help" "../doom-core/autoload/help.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/help.el

(autoload 'doom/describe-setting "../doom-core/autoload/help" "\
Open the documentation of SETTING (a keyword defined with `def-setting!').

\(fn SETTING)" t nil)

(autoload 'doom/describe-module "../doom-core/autoload/help" "\
Open the documentation of MODULE (a string that represents the category and
submodule in the format, e.g. ':feature evil').

\(fn MODULE)" t nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/memoize" "../doom-core/autoload/memoize.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/memoize.el

(defvar doom-memoized-table (make-hash-table :test 'equal :size 10) "\
A lookup table containing memoized functions. The keys are argument lists,
and the value is the function's return value.")

(autoload 'doom-memoize "../doom-core/autoload/memoize" "\
Memoizes an existing function. NAME is a symbol.

\(fn NAME)" nil nil)

(autoload 'def-memoized! "../doom-core/autoload/memoize" "\
Create a memoize'd function. NAME, ARGLIST, DOCSTRING and BODY
have the same meaning as in `defun'.

\(fn NAME ARGLIST &rest BODY)" nil t)

(function-put 'def-memoized! 'lisp-indent-function 'defun)

(function-put 'def-memoized! 'doc-string-elt '3)

;;;***

;;;### (autoloads nil "../doom-core/autoload/menu" "../doom-core/autoload/menu.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/menu.el

(autoload 'def-menu! "../doom-core/autoload/menu" "\
Defines a menu and returns a function symbol for invoking it.

A dispatcher is an interactive command named NAME (a symbol). When called, this
dispatcher prompts you to select a command to run. This list is filtered
depending on its properties. Each command is takes the form of:

  (DESCRIPTION :exec COMMAND &rest PROPERTIES)

PROPERTIES accepts the following properties:

  :when FORM
  :unless FORM
  :region BOOL
  :cwd t|PATH
  :project BOOL|DIRECTORY

COMMAND can be a string (a shell command), a symbol (an elisp function) or a
lisp form.

`def-menu!'s PLIST supports the following properties:

  :prompt STRING

\(fn NAME DESC COMMANDS &rest PLIST)" nil t)

(function-put 'def-menu! 'lisp-indent-function 'defun)

(function-put 'def-menu! 'doc-string-elt '2)

;;;***

;;;### (autoloads nil "../doom-core/autoload/message" "../doom-core/autoload/message.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/message.el

(autoload 'format! "../doom-core/autoload/message" "\
An alternative to `format' that strips out ANSI codes if used in an
interactive session.

\(fn MESSAGE &rest ARGS)" nil t)

(autoload 'message! "../doom-core/autoload/message" "\
An alternative to `message' that strips out ANSI codes if used in an
interactive session.

\(fn MESSAGE &rest ARGS)" nil t)

(autoload 'debug! "../doom-core/autoload/message" "\
Out a debug message if `doom-debug-mode' is non-nil. Otherwise, ignore this.

\(fn MESSAGE &rest ARGS)" nil t)

(autoload 'doom-ansi-apply "../doom-core/autoload/message" "\


\(fn CODE FORMAT &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/minibuffer" "../doom-core/autoload/minibuffer.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/minibuffer.el

(autoload 'doom/minibuffer-kill-word "../doom-core/autoload/minibuffer" "\
Kill a word, backwards, but only if the cursor is after
`minibuffer-prompt-end', to prevent the 'Text is read-only' warning from
monopolizing the minibuffer.

\(fn)" t nil)

(autoload 'doom/minibuffer-kill-line "../doom-core/autoload/minibuffer" "\
Kill the entire line, but only if the cursor is after
`minibuffer-prompt-end', to prevent the 'Text is read-only' warning from
monopolizing the minibuffer.

\(fn)" t nil)

(autoload 'doom/minibuffer-undo "../doom-core/autoload/minibuffer" "\
Undo an edit in the minibuffer without throwing errors.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/packages" "../doom-core/autoload/packages.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/packages.el

(autoload 'doom-refresh-packages "../doom-core/autoload/packages" "\
Refresh ELPA packages.

\(fn &optional FORCE-P)" nil nil)

(autoload 'doom-refresh-clear-cache "../doom-core/autoload/packages" "\
Clear the cache for `doom-refresh-packages'.

\(fn)" nil nil)

(autoload 'doom-package-backend "../doom-core/autoload/packages" "\
Get which backend the package NAME was installed with. Can either be elpa or
quelpa. Throws an error if NOERROR is nil and the package isn't installed.

\(fn NAME &optional NOERROR)" nil nil)

(autoload 'doom-package-outdated-p "../doom-core/autoload/packages" "\
Determine whether NAME (a symbol) is outdated or not. If outdated, returns a
list, whose car is NAME, and cdr the current version list and latest version
list of the package.

\(fn NAME)" nil nil)

(autoload 'doom-package-prop "../doom-core/autoload/packages" "\
Return PROPerty in NAME's plist.

\(fn NAME PROP)" nil nil)

(autoload 'doom-package-different-backend-p "../doom-core/autoload/packages" "\
Return t if NAME (a package's symbol) has a new backend than what it was
installed with. Returns nil otherwise, or if package isn't installed.

\(fn NAME)" nil nil)

(autoload 'doom-get-packages "../doom-core/autoload/packages" "\
Retrieves a list of explicitly installed packages (i.e. non-dependencies).
Each element is a cons cell, whose car is the package symbol and whose cdr is
the quelpa recipe (if any).

BACKEND can be 'quelpa or 'elpa, and will instruct this function to return only
the packages relevant to that backend.

Warning: this function is expensive; it re-evaluates all of doom's config files.
Be careful not to use it in a loop.

If INSTALLED-ONLY-P, only return packages that are installed.

\(fn &optional INSTALLED-ONLY-P)" nil nil)

(autoload 'doom-get-depending-on "../doom-core/autoload/packages" "\
Return a list of packages that depend on the package named NAME.

\(fn NAME)" nil nil)

(autoload 'doom-get-dependencies-for "../doom-core/autoload/packages" "\
Return a list of dependencies for a package.

\(fn NAME &optional ONLY)" nil nil)

(autoload 'doom-get-outdated-packages "../doom-core/autoload/packages" "\
Return a list of packages that are out of date. Each element is a list,
containing (PACKAGE-SYMBOL OLD-VERSION-LIST NEW-VERSION-LIST).

If INCLUDE-FROZEN-P is non-nil, check frozen packages as well.

Used by `doom//packages-update'.

\(fn &optional INCLUDE-FROZEN-P)" nil nil)

(autoload 'doom-get-orphaned-packages "../doom-core/autoload/packages" "\
Return a list of symbols representing packages that are no longer needed or
depended on.

Used by `doom//packages-autoremove'.

\(fn)" nil nil)

(autoload 'doom-get-missing-packages "../doom-core/autoload/packages" "\
Return a list of requested packages that aren't installed or built-in, but
are enabled (with a `package!' directive). Each element is a list whose CAR is
the package symbol, and whose CDR is a plist taken from that package's
`package!' declaration.

If INCLUDE-IGNORED-P is non-nil, includes missing packages that are ignored,
i.e. they have an :ignore property.

Used by `doom//packages-install'.

\(fn &optional INCLUDE-IGNORED-P)" nil nil)

(autoload 'doom*package-delete "../doom-core/autoload/packages" "\
Update `quelpa-cache' upon a successful `package-delete'.

\(fn DESC &rest _)" nil nil)

(autoload 'doom//packages-install "../doom-core/autoload/packages" "\
Interactive command for installing missing packages.

\(fn)" t nil)

(autoload 'doom//packages-update "../doom-core/autoload/packages" "\
Interactive command for updating packages.

\(fn)" t nil)

(autoload 'doom//packages-autoremove "../doom-core/autoload/packages" "\
Interactive command for auto-removing orphaned packages.

\(fn)" t nil)

(defalias 'doom/install-package #'package-install)

(autoload 'doom/reinstall-package "../doom-core/autoload/packages" "\
Reinstalls package package with optional quelpa RECIPE (see `quelpa-recipe' for
an example; the package package can be omitted).

\(fn DESC)" t nil)

(function-put 'doom/reinstall-package 'interactive-only 't)

(autoload 'doom/delete-package "../doom-core/autoload/packages" "\
Prompts the user with a list of packages and deletes the selected package.
Use this interactively. Use `doom-delete-package' for direct calls.

\(fn DESC)" t nil)

(function-put 'doom/delete-package 'interactive-only 't)

(autoload 'doom/update-package "../doom-core/autoload/packages" "\
Prompts the user with a list of outdated packages and updates the selected
package. Use this interactively. Use `doom-update-package' for direct
calls.

\(fn PKG)" t nil)

(function-put 'doom/update-package 'interactive-only 't)

(autoload 'doom/refresh-packages "../doom-core/autoload/packages" "\
Synchronize package metadata with the sources in `package-archives'. If
FORCE-P (the universal argument) is set, ignore the cache.

\(fn &optional FORCE-P)" t nil)

(function-put 'doom/refresh-packages 'interactive-only 't)

;;;***

;;;### (autoloads nil "../doom-core/autoload/popups" "../doom-core/autoload/popups.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/popups.el

(autoload 'doom-popup-p "../doom-core/autoload/popups" "\
Return t if TARGET (a window or buffer) is a popup. Uses current window if
omitted.

\(fn &optional TARGET)" nil nil)

(autoload 'doom-popup-buffer "../doom-core/autoload/popups" "\
Display BUFFER in a shackle popup with PLIST rules. See `shackle-rules' for
possible rules. If EXTEND-P is non-nil, don't overwrite the original rules for
this popup, just the specified properties. Returns the new popup window.

\(fn BUFFER &optional PLIST EXTEND-P)" nil nil)

(function-put 'doom-popup-buffer 'lisp-indent-function 'defun)

(autoload 'doom-popup-switch-to-buffer "../doom-core/autoload/popups" "\
Switch the current (or closest) pop-up window to BUFFER.

\(fn BUFFER)" nil nil)

(autoload 'doom-popup-fit-to-buffer "../doom-core/autoload/popups" "\
Fit WINDOW to the size of its content.

\(fn &optional WINDOW MAX-SIZE)" nil nil)

(autoload 'doom-popup-move "../doom-core/autoload/popups" "\
Move a popup window to another side of the frame, in DIRECTION, which can be
one of the following: 'left 'right 'above 'below

\(fn DIRECTION)" nil nil)

(autoload 'doom-popup-file "../doom-core/autoload/popups" "\
Display FILE in a shackle popup, with PLIST rules. See `shackle-rules' for
possible rules.

\(fn FILE &optional PLIST EXTEND-P)" nil nil)

(autoload 'doom-popup-windows "../doom-core/autoload/popups" "\
Get a list of open pop up windows.

\(fn &optional FILTER-STATIC-P)" nil nil)

(autoload 'doom-popup-properties "../doom-core/autoload/popups" "\
Returns a window's popup property list, if possible. The buffer-local
`doom-popup-rules' always takes priority, but this will fall back to the popup
window parameter.

\(fn WINDOW-OR-BUFFER)" nil nil)

(autoload 'doom-popup-property "../doom-core/autoload/popups" "\
Returns a `doom-popup-rules' PROPerty from WINDOW.

\(fn PROP &optional WINDOW)" nil nil)

(autoload 'doom-popup-side "../doom-core/autoload/popups" "\
Return what side a popup WINDOW came from ('left 'right 'above or 'below).

\(fn &optional WINDOW)" nil nil)

(autoload 'doom-popup-size "../doom-core/autoload/popups" "\
Return the size of a popup WINDOW.

\(fn &optional WINDOW)" nil nil)

(autoload 'with-popup-rules! "../doom-core/autoload/popups" "\
TODO

\(fn RULES &rest BODY)" nil t)

(function-put 'with-popup-rules! 'lisp-indent-function 'defun)

(autoload 'save-popups! "../doom-core/autoload/popups" "\
Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa).

\(fn &rest BODY)" nil t)

(autoload 'doom/popup-restore "../doom-core/autoload/popups" "\
Restore the last open popups. If the buffers have been killed, and
represented real files, they will be restored. Dead special buffers or buffers
with non-nil :autokill properties will not be.

Returns t if popups were restored, nil otherwise.

\(fn)" t nil)

(autoload 'doom/popup-toggle "../doom-core/autoload/popups" "\
Toggle popups on and off. If used outside of popups (and popups are
available), it will select the nearest popup window.

\(fn)" t nil)

(autoload 'doom/popup-close "../doom-core/autoload/popups" "\
Find and close WINDOW if it's a popup. If WINDOW is omitted, defaults to
`selected-window'. The contained buffer is buried, unless it has an :autokill
property.

\(fn &optional WINDOW)" t nil)

(autoload 'doom/popup-close-all "../doom-core/autoload/popups" "\
Closes most open popups.

Does not close popups that are :static or don't have an :autoclose property (see
`shackle-rules').

If FORCE-P is non-nil (or this function is called interactively), ignore popups'
:autoclose property. This command will never close :static popups.

\(fn &optional FORCE-P)" t nil)

(autoload 'doom/popup-kill-all "../doom-core/autoload/popups" "\
Like `doom/popup-close-all', but kill *all* popups, including :static ones,
without leaving any trace behind (muahaha).

\(fn)" t nil)

(autoload 'doom/popup-close-maybe "../doom-core/autoload/popups" "\
Close the current popup *if* its window doesn't have a noesc parameter.

\(fn)" t nil)

(autoload 'doom/popup-this-buffer "../doom-core/autoload/popups" "\
Display currently selected buffer in a popup window.

\(fn)" t nil)

(autoload 'doom/popup-toggle-messages "../doom-core/autoload/popups" "\
Toggle *Messages* buffer.

\(fn)" t nil)

(autoload 'doom/other-popup "../doom-core/autoload/popups" "\
Cycle through popup windows. Like `other-window', but for popups.

\(fn COUNT)" t nil)

(defalias 'other-popup #'doom/other-popup)

(autoload 'doom/popup-raise "../doom-core/autoload/popups" "\
Turn a popup window into a normal window.

\(fn &optional WINDOW)" t nil)

(autoload 'doom/popup-move-top "../doom-core/autoload/popups" "\
See `doom-popup-move'.

\(fn)" t nil)

(autoload 'doom/popup-move-bottom "../doom-core/autoload/popups" "\
See `doom-popup-move'.

\(fn)" t nil)

(autoload 'doom/popup-move-left "../doom-core/autoload/popups" "\
See `doom-popup-move'.

\(fn)" t nil)

(autoload 'doom/popup-move-right "../doom-core/autoload/popups" "\
See `doom-popup-move'.

\(fn)" t nil)

(autoload 'doom-popup-mode "../doom-core/autoload/popups" "\
Minor mode for popup windows.

\(fn &optional ARG)" t nil)

(autoload 'doom|hide-modeline-in-popup "../doom-core/autoload/popups" "\
Don't show modeline in popup windows without a :modeline rule. If one exists
and it's a symbol, use `doom-modeline' to grab the format. If non-nil, show the
mode-line as normal. If nil (or omitted, by default), then hide the modeline
entirely.

\(fn)" nil nil)

(autoload 'doom*shackle-always-align "../doom-core/autoload/popups" "\
Ensure popups are always aligned and selected by default. Eliminates the need
for :align t on every rule.

\(fn PLIST)" nil nil)

(autoload 'doom*popup-init "../doom-core/autoload/popups" "\
Initializes a window as a popup window by enabling `doom-popup-mode' in it
and setting `doom-popup-rules' within it. Returns the window.

\(fn ORIG-FN &rest ARGS)" nil nil)

(autoload 'doom*popups-save "../doom-core/autoload/popups" "\
Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa).

\(fn ORIG-FN &rest ARGS)" nil nil)

(autoload 'doom*delete-popup-window "../doom-core/autoload/popups" "\
Ensure that popups are deleted properly, and killed if they have :autokill
properties.

\(fn &optional WINDOW)" nil nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/scratch" "../doom-core/autoload/scratch.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/scratch.el

(autoload 'doom/open-scratch-buffer "../doom-core/autoload/scratch" "\
Opens a temporary scratch buffer in a popup window. It is discarded once it
is closed. If a region is active, copy it to the scratch buffer.

\(fn)" t nil)

(autoload 'doom/open-project-scratch-buffer "../doom-core/autoload/scratch" "\
Opens a (persistent) scratch buffer associated with the current project in a
popup window. Scratch buffers are stored in `doom-scratch-files-dir'. If a
region is active, copy it to the scratch buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/system" "../doom-core/autoload/system.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/system.el

(autoload 'doom-system-os "../doom-core/autoload/system" "\
Returns the OS: arch, debian, macos, general linux, cygwin or windows. If OS
is given, returns t if it matches the current system, and nil otherwise.

\(fn &optional OS)" nil nil)

(autoload 'doom-sh "../doom-core/autoload/system" "\
Runs a shell command and prints any output to the DOOM buffer.

\(fn COMMAND &rest ARGS)" nil nil)

(autoload 'doom-sudo "../doom-core/autoload/system" "\
Like `doom-sh', but runs as root (prompts for password).

\(fn COMMAND &rest ARGS)" nil nil)

(autoload 'doom-fetch "../doom-core/autoload/system" "\
Clone a remote version-controlled repo at REPO-URL to PATH, if it exists.
Requires the corresponding client, e.g. git for git repos, hg for mercurial,
etc.

\(fn FETCHER LOCATION DEST)" nil nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/test" "../doom-core/autoload/test.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/test.el

(autoload 'doom//run-tests "../doom-core/autoload/test" "\
Run all loaded tests, specified by MODULES (a list of module cons cells) or
command line args following a double dash (each arg should be in the
'module/submodule' format).

If neither is available, run all tests in all enabled modules.

\(fn &optional MODULES)" t nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/ui" "../doom-core/autoload/ui.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/ui.el

(autoload 'doom/toggle-fullscreen "../doom-core/autoload/ui" "\
Toggle fullscreen Emacs (non-native on MacOS).

\(fn)" t nil)

(autoload 'doom/toggle-line-numbers "../doom-core/autoload/ui" "\
Toggle `linum-mode'.

\(fn &optional ARG)" t nil)

(autoload 'doom-resize-window "../doom-core/autoload/ui" "\
Resize a window to NEW-SIZE. If HORIZONTAL, do it width-wise.
If FORCE-P is omitted when `window-size-fixed' is non-nil, resizing will fail.

\(fn WINDOW NEW-SIZE &optional HORIZONTAL FORCE-P)" nil nil)

(autoload 'doom/window-zoom "../doom-core/autoload/ui" "\
Close other windows to focus on this one. Activate again to undo this. If the
window changes before then, the undo expires.

Alternatively, use `doom/window-enlargen'.

\(fn)" t nil)

(autoload 'doom/window-enlargen "../doom-core/autoload/ui" "\
Enlargen the current window to focus on this one. Does not close other
windows (unlike `doom/window-zoom') Activate again to undo.

\(fn)" t nil)

(autoload 'doom/delete-frame "../doom-core/autoload/ui" "\
Delete the current frame, but ask for confirmation if it isn't empty.

\(fn)" t nil)

(defvar doom-big-font-mode nil "\
Non-nil if Doom-Big-Font mode is enabled.
See the `doom-big-font-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `doom-big-font-mode'.")

(custom-autoload 'doom-big-font-mode "../doom-core/autoload/ui" nil)

(autoload 'doom-big-font-mode "../doom-core/autoload/ui" "\
A global mode that resizes the font, for streams, screen-sharing and
presentations.

\(fn &optional ARG)" t nil)

(autoload 'doom//reload-theme "../doom-core/autoload/ui" "\
Reset the color theme currently in use.

\(fn)" t nil)

(autoload 'hydra-move-splitter-left "../doom-core/autoload/ui" "\
Move window splitter left.

\(fn ARG)" t nil)

(autoload 'hydra-move-splitter-right "../doom-core/autoload/ui" "\
Move window splitter right.

\(fn ARG)" t nil)

(autoload 'hydra-move-splitter-up "../doom-core/autoload/ui" "\
Move window splitter up.

\(fn ARG)" t nil)

(autoload 'hydra-move-splitter-down "../doom-core/autoload/ui" "\
Move window splitter down.

\(fn ARG)" t nil)

(autoload 'fast-kill-buffer "../doom-core/autoload/ui" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "../doom-core/autoload/utiltiy" "../doom-core/autoload/utiltiy.el"
;;;;;;  (23321 60608 393015 171000))
;;; Generated autoloads from ../doom-core/autoload/utiltiy.el

(autoload 'occur-step "../doom-core/autoload/utiltiy" "\


\(fn ARG)" nil nil)

(autoload 'occur-step-forward "../doom-core/autoload/utiltiy" "\


\(fn ARG)" t nil)

(autoload 'occur-step-backward "../doom-core/autoload/utiltiy" "\


\(fn ARG)" t nil)

(autoload 'find-file-init-dot-el "../doom-core/autoload/utiltiy" "\


\(fn)" t nil)

(autoload 'find-file-config-dot-org "../doom-core/autoload/utiltiy" "\


\(fn)" nil nil)

(autoload 'find-buffer-messages-buffer "../doom-core/autoload/utiltiy" "\


\(fn)" nil nil)

(autoload 'find-buffer-scratch-buffer "../doom-core/autoload/utiltiy" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "../modules/core-ui/company/autoload" "../modules/core-ui/company/autoload.el"
;;;;;;  (23321 60608 397014 876000))
;;; Generated autoloads from ../modules/core-ui/company/autoload.el

(autoload '+company/complete "../modules/core-ui/company/autoload" "\
Bring up the completion popup. If only one result, complete it.

\(fn)" t nil)

(autoload '+company/whole-lines "../modules/core-ui/company/autoload" "\
`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload '+company/dict-or-keywords "../modules/core-ui/company/autoload" "\
`company-mode' completion combining `company-dict' and `company-keywords'.

\(fn)" t nil)

(autoload '+company/dabbrev-code-previous "../modules/core-ui/company/autoload" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/core-ui/ido/autoload/ido" "../modules/core-ui/ido/autoload/ido.el"
;;;;;;  (23321 60608 401014 581000))
;;; Generated autoloads from ../modules/core-ui/ido/autoload/ido.el

(autoload '+smex "../modules/core-ui/ido/autoload/ido" "\
Like `smex', but use `small-ido-max-window-height' to
limit the buffer size.`

\(fn)" t nil)

(autoload '+ido-find-file "../modules/core-ui/ido/autoload/ido" "\
Like `ido-find-file', but use `small-ido-max-window-height' to
limit the buffer size.`

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/core-ui/ivy/autoload/ivy" "../modules/core-ui/ivy/autoload/ivy.el"
;;;;;;  (23321 60608 401014 581000))
;;; Generated autoloads from ../modules/core-ui/ivy/autoload/ivy.el

(autoload '+ivy-buffer-transformer "../modules/core-ui/ivy/autoload/ivy" "\


\(fn STR)" nil nil)

(autoload '+ivy/switch-workspace-buffer "../modules/core-ui/ivy/autoload/ivy" "\
Switch to another buffer within the current workspace.

If ARG (universal argument), open selection in other-window.

\(fn &optional ARG)" t nil)

(autoload '+ivy/tasks "../modules/core-ui/ivy/autoload/ivy" "\
Search through all TODO/FIXME tags in the current project. If ARG, only
search current file. See `+ivy-task-tags' to customize what this searches for.

\(fn &optional ARG)" t nil)

(autoload '+ivy*counsel-ag-function "../modules/core-ui/ivy/autoload/ivy" "\
Advice to 1) get rid of the character limit from `counsel-ag-function' and 2)
disable ivy's over-zealous parentheses quoting behavior (if i want literal
parentheses, I'll escape them myself).

NOTE This may need to be updated frequently, to meet changes upstream (in
counsel-rg).

\(fn STRING BASE-CMD EXTRA-AG-ARGS)" nil nil)

(autoload '+ivy/wgrep-occur "../modules/core-ui/ivy/autoload/ivy" "\
Invoke the search+replace wgrep buffer on the current ag/rg search results.

\(fn)" t nil)

(autoload '+ivy-yas-prompt "../modules/core-ui/ivy/autoload/ivy" "\


\(fn PROMPT CHOICES &optional DISPLAY-FN)" nil nil)

(autoload '+ivy-git-grep-other-window-action "../modules/core-ui/ivy/autoload/ivy" "\
Opens the current candidate in another window.

\(fn X)" nil nil)

(autoload '+ivy-quit-and-resume "../modules/core-ui/ivy/autoload/ivy" "\
Close the current popup window and resume ivy.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/feature/eval/autoload/eval" "../modules/feature/eval/autoload/eval.el"
;;;;;;  (23321 60608 401014 581000))
;;; Generated autoloads from ../modules/feature/eval/autoload/eval.el

(autoload '+eval/buffer "../modules/feature/eval/autoload/eval" "\
Evaluate the whole buffer.

\(fn)" t nil)

(autoload '+eval/region "../modules/feature/eval/autoload/eval" "\
Evaluate a region between BEG and END and display the output.

\(fn BEG END)" t nil)

(autoload '+eval/region-and-replace "../modules/feature/eval/autoload/eval" "\
Evaluation a region between BEG and END, and replace it with the result.

\(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil "../modules/feature/eval/autoload/repl" "../modules/feature/eval/autoload/repl.el"
;;;;;;  (23321 60608 401014 581000))
;;; Generated autoloads from ../modules/feature/eval/autoload/repl.el

(autoload '+eval/open-repl "../modules/feature/eval/autoload/repl" "\
Opens (or reopens) the REPL associated with the current major-mode and place
the cursor at the prompt.

\(fn)" t nil)

(autoload '+eval/send-region-to-repl "../modules/feature/eval/autoload/repl" "\
REPL must be open! Sends a selected region to it. If AUTO-EXECUTE-P, then
execute it immediately after.

\(fn BEG END &optional AUTO-EXECUTE-P)" t nil)

;;;***

;;;### (autoloads nil "../modules/feature/jump/autoload/jump" "../modules/feature/jump/autoload/jump.el"
;;;;;;  (23321 60608 405014 285000))
;;; Generated autoloads from ../modules/feature/jump/autoload/jump.el

(autoload '+jump/definition "../modules/feature/jump/autoload/jump" "\
Jump to the definition of the symbol at point.

Tries xref and falls back to `dumb-jump', then rg/ag, then
`evil-goto-definition' (if evil is active).

\(fn IDENTIFIER &optional OTHER-WINDOW)" t nil)

(autoload '+jump/references "../modules/feature/jump/autoload/jump" "\
Show a list of references to the symbol at point.

Tries `xref-find-references' and falls back to rg/ag.

\(fn IDENTIFIER)" t nil)

(autoload '+jump/documentation "../modules/feature/jump/autoload/jump" "\
Show documentation for the symbol at point, if available.

\(fn IDENTIFIER)" t nil)

(autoload '+jump/online "../modules/feature/jump/autoload/jump" "\
Looks up SEARCH (a string) in you browser using PROVIDER.

PROVIDER should be a key of `+jump-search-provider-alist'.

When used interactively, it will prompt for a query and, for the first time, the
provider from `+jump-search-provider-alist'. On consecutive uses, the last
provider will be reused. If the universal argument is supplied, always prompt
for the provider.

\(fn SEARCH &optional PROVIDER)" t nil)

(autoload '+jump/online-select "../modules/feature/jump/autoload/jump" "\
Runs `+jump/online', but always prompts for the provider to use.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/feature/snippets/autoload/snippets"
;;;;;;  "../modules/feature/snippets/autoload/snippets.el" (23321
;;;;;;  60608 405014 285000))
;;; Generated autoloads from ../modules/feature/snippets/autoload/snippets.el

(autoload '+snippets/goto-start-of-field "../modules/feature/snippets/autoload/snippets" "\
Go to the beginning of the current field.

\(fn)" t nil)

(autoload '+snippets/goto-end-of-field "../modules/feature/snippets/autoload/snippets" "\
Go to the end of the current field.

\(fn)" t nil)

(autoload '+snippets/delete-backward-char "../modules/feature/snippets/autoload/snippets" "\
Prevents Yas from interfering with backspace deletion.

\(fn &optional FIELD)" t nil)

(autoload '+snippets/delete-forward-char-or-field "../modules/feature/snippets/autoload/snippets" "\
Delete forward, or skip the current field if it's empty. This is to prevent
buggy behavior when <delete> is pressed in an empty field.

\(fn &optional FIELD)" t nil)

(autoload '+snippets/delete-to-start-of-field "../modules/feature/snippets/autoload/snippets" "\
Delete to start-of-field.

\(fn &optional FIELD)" t nil)

;;;***

;;;### (autoloads nil "../modules/feature/version-control/autoload"
;;;;;;  "../modules/feature/version-control/autoload.el" (23321 60608
;;;;;;  405014 285000))
;;; Generated autoloads from ../modules/feature/version-control/autoload.el

(autoload '+vcs-root "../modules/feature/version-control/autoload" "\
Get git url root.

\(fn)" nil nil)

(autoload '+vcs/git-browse "../modules/feature/version-control/autoload" "\
Open the website for the current version controlled file. Fallback to
repository root.

\(fn)" t nil)

(autoload '+vcs/git-browse-issues "../modules/feature/version-control/autoload" "\
Open the github issues page for current repo.

\(fn)" t nil)

(autoload '+vcs|init-header-line "../modules/feature/version-control/autoload" "\
Toggle the git-timemachine header-line on activate. Use this on
`git-timemachine-mode-hook'.

\(fn)" nil nil)

(autoload '+vcs|enable-smerge-mode-maybe "../modules/feature/version-control/autoload" "\
Auto-enable `smerge-mode' when merge conflict is detected.

\(fn)" nil nil)

(autoload '+vcs*update-header-line "../modules/feature/version-control/autoload" "\
Show revision details in the header-line, instead of the minibuffer.

Sometimes I forget `git-timemachine' is enabled in a buffer. Putting info into,
putting them in `header-line-format' has better visibility.

\(fn &rest _)" nil nil)

;;;***

;;;### (autoloads nil "../modules/feature/workspaces/autoload/workspaces"
;;;;;;  "../modules/feature/workspaces/autoload/workspaces.el" (23321
;;;;;;  60608 405014 285000))
;;; Generated autoloads from ../modules/feature/workspaces/autoload/workspaces.el

(defalias #'+workspace-p #'persp-p "\
Return t if OBJ is a perspective hash table.")

(autoload '+workspace-exists-p "../modules/feature/workspaces/autoload/workspaces" "\
Returns t if NAME is the name of an existing workspace.

\(fn NAME)" nil nil)

(autoload '+workspace-contains-buffer-p "../modules/feature/workspaces/autoload/workspaces" "\
Return non-nil if buffer is in workspace (defaults to current workspace).

\(fn BUFFER &optional WORKSPACE)" nil nil)

(autoload '+workspace-get "../modules/feature/workspaces/autoload/workspaces" "\
Returns a workspace (perspective hash table) named NAME.

\(fn NAME &optional NOERROR)" nil nil)

(defalias '+workspace-current #'get-current-persp)

(autoload '+workspace-current-name "../modules/feature/workspaces/autoload/workspaces" "\
Get the name of the current workspace.

\(fn)" nil nil)

(autoload '+workspace-list "../modules/feature/workspaces/autoload/workspaces" "\
Return a list of workspace structs.

\(fn)" nil nil)

(autoload '+workspace-list-names "../modules/feature/workspaces/autoload/workspaces" "\
Return a list of workspace names (strings).

\(fn)" nil nil)

(autoload '+workspace-buffer-list "../modules/feature/workspaces/autoload/workspaces" "\
Return a list of buffers in PERSP (defaults to the current perspective).

The buffer list is ordered by recency (same as `buffer-list').

PERSP can be a string (name of a workspace) or a perspective hash (satisfies
`+workspace-p').

If PERSP is t, then return a list of orphaned buffers associated with no
perspectives.

\(fn &optional PERSP)" nil nil)

(autoload '+workspace-load "../modules/feature/workspaces/autoload/workspaces" "\
Loads a single workspace (named NAME) into the current session. Can only
retrieve perspectives that were explicitly saved with `+workspace-save'.

Returns t if successful, nil otherwise.

\(fn NAME)" nil nil)

(autoload '+workspace-load-session "../modules/feature/workspaces/autoload/workspaces" "\
Replace current session with the entire session named NAME. If NAME is nil,
use `persp-auto-save-fname'.

\(fn &optional NAME)" nil nil)

(autoload '+workspace-save "../modules/feature/workspaces/autoload/workspaces" "\
Saves a single workspace (NAME) from the current session. Can be loaded again
with `+workspace-load'. NAME can be the string name of a workspace or its
perspective hash table.

Returns t on success, nil otherwise.

\(fn NAME)" nil nil)

(autoload '+workspace-save-session "../modules/feature/workspaces/autoload/workspaces" "\
Save a whole session as NAME. If NAME is nil, use `persp-auto-save-fname'.
Return t on success, nil otherwise.

\(fn &optional NAME)" nil nil)

(autoload '+workspace-new "../modules/feature/workspaces/autoload/workspaces" "\
Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise.

\(fn NAME)" nil nil)

(autoload '+workspace-rename "../modules/feature/workspaces/autoload/workspaces" "\
Rename the current workspace named NAME to NEW-NAME. Returns old name on
success, nil otherwise.

\(fn NAME NEW-NAME)" nil nil)

(autoload '+workspace-delete "../modules/feature/workspaces/autoload/workspaces" "\
Delete the workspace denoted by NAME, which can be the name of a perspective
or its hash table. If INHIBIT-KILL-P is non-nil, don't kill this workspace's
buffers.

\(fn NAME &optional INHIBIT-KILL-P)" nil nil)

(autoload '+workspace-switch "../modules/feature/workspaces/autoload/workspaces" "\
Switch to another workspace.

\(fn NAME &optional AUTO-CREATE-P)" nil nil)

(autoload '+workspace/load "../modules/feature/workspaces/autoload/workspaces" "\
Load a workspace and switch to it. If called with C-u, try to reload the
current workspace (by name) from session files.

\(fn NAME)" t nil)

(autoload '+workspace/load-session "../modules/feature/workspaces/autoload/workspaces" "\
Load a session and switch to it. If called with C-u, try to load the last
session.

\(fn &optional NAME)" t nil)

(autoload '+workspace/save "../modules/feature/workspaces/autoload/workspaces" "\
Save the current workspace. If called with C-u, autosave the current
workspace.

\(fn NAME)" t nil)

(autoload '+workspace/save-session "../modules/feature/workspaces/autoload/workspaces" "\
Save the current session. If called with C-u, prompt you for the name to save
the session as.

\(fn &optional NAME)" t nil)

(autoload '+workspace/rename "../modules/feature/workspaces/autoload/workspaces" "\
Rename the current workspace.

\(fn NEW-NAME)" t nil)

(autoload '+workspace/delete "../modules/feature/workspaces/autoload/workspaces" "\
Delete this workspace. If called with C-u, prompts you for the name of the
workspace to delete.

\(fn NAME)" t nil)

(autoload '+workspace/kill-session "../modules/feature/workspaces/autoload/workspaces" "\
Delete the current session, clears all workspaces, windows and buffers.

\(fn)" t nil)

(autoload '+workspace/kill-session-and-quit "../modules/feature/workspaces/autoload/workspaces" "\
Forgets current session and quits.

\(fn)" t nil)

(autoload '+workspace/new "../modules/feature/workspaces/autoload/workspaces" "\
Create a new workspace named NAME. If OVERWRITE-P is non-nil, clear any
pre-existing workspace.

\(fn &optional NAME CLONE-P)" t nil)

(autoload '+workspace/switch-to "../modules/feature/workspaces/autoload/workspaces" "\
Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list.

\(fn INDEX)" t nil)

(autoload '+workspace/switch-to-last "../modules/feature/workspaces/autoload/workspaces" "\
Switch to the last workspace.

\(fn)" t nil)

(autoload '+workspace/cycle "../modules/feature/workspaces/autoload/workspaces" "\
Cycle n workspaces to the right (default) or left.

\(fn N)" t nil)

(autoload '+workspace/switch-left "../modules/feature/workspaces/autoload/workspaces" "\


\(fn)" t nil)

(autoload '+workspace/switch-right "../modules/feature/workspaces/autoload/workspaces" "\


\(fn)" t nil)

(autoload '+workspace/close-window-or-workspace "../modules/feature/workspaces/autoload/workspaces" "\
Close the selected window. If it's the last window in the workspace, close
the workspace and move to the next.

\(fn)" t nil)

(autoload '+workspace/close-workspace-or-frame "../modules/feature/workspaces/autoload/workspaces" "\
Close the current workspace. If it's the last, delete the frame instead.

\(fn)" t nil)

(autoload '+workspace-message "../modules/feature/workspaces/autoload/workspaces" "\
Show an 'elegant' message in the echo area next to a listing of workspaces.

\(fn MESSAGE &optional TYPE)" nil nil)

(autoload '+workspace-error "../modules/feature/workspaces/autoload/workspaces" "\
Show an 'elegant' error in the echo area next to a listing of workspaces.

\(fn MESSAGE &optional NOERROR)" nil nil)

(autoload '+workspace/display "../modules/feature/workspaces/autoload/workspaces" "\
Display a list of workspaces (like tabs) in the echo area.

\(fn)" t nil)

(autoload '+workspace-on-new-frame "../modules/feature/workspaces/autoload/workspaces" "\
Spawn a perspective for each new frame.

\(fn FRAME &optional NEW-FRAME-P)" nil nil)

(autoload '+workspaces|delete-associated-workspace-maybe "../modules/feature/workspaces/autoload/workspaces" "\
Delete workspace associated with current frame IF it has no real buffers.

\(fn FRAME)" nil nil)

(autoload '+workspaces*autosave-real-buffers "../modules/feature/workspaces/autoload/workspaces" "\
Don't autosave if no real buffers are open.

\(fn ORIG-FN &rest ARGS)" nil nil)

(autoload '+workspaces*switch-project-by-name "../modules/feature/workspaces/autoload/workspaces" "\
Switch to a project and prompt for a file to open.

Ensures the scratch (or dashboard) buffers are CDed into the project's root.

\(fn ORIG-FN &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "../modules/gui/hl-todo/autoload" "../modules/gui/hl-todo/autoload.el"
;;;;;;  (23321 60608 405014 285000))
;;; Generated autoloads from ../modules/gui/hl-todo/autoload.el

(autoload '+hl-todo|use-face-detection "../modules/gui/hl-todo/autoload" "\
Use a different, more primitive method of locating todo keywords.

This is useful for major modes that don't use or have a valid syntax-table entry
for comment start/end characters.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "../modules/gui/nav-flash/autoload" "../modules/gui/nav-flash/autoload.el"
;;;;;;  (23321 60608 409013 990000))
;;; Generated autoloads from ../modules/gui/nav-flash/autoload.el

(autoload '+doom*blink-cursor-maybe "../modules/gui/nav-flash/autoload" "\
Blink current line if the window has moved.

\(fn ORIG-FN &rest ARGS)" nil nil)

(autoload '+doom/blink-cursor "../modules/gui/nav-flash/autoload" "\
Blink current line using `nav-flash'.

\(fn &rest _)" t nil)

;;;***

;;;### (autoloads nil "../modules/lang/cc/autoload" "../modules/lang/cc/autoload.el"
;;;;;;  (23314 53633 724863 900000))
;;; Generated autoloads from ../modules/lang/cc/autoload.el

(autoload '+cc/reload-compile-db "../modules/lang/cc/autoload" "\
Reload the current project's JSON compilation database.

\(fn &optional FORCE-P)" t nil)

(autoload '+cc*align-lambda-arglist "../modules/lang/cc/autoload" "\
Improve indentation of continued C++11 lambda function opened as argument.

\(fn ORIG-FUN &rest ARGS)" nil nil)

(autoload '+cc/autoclose->-maybe "../modules/lang/cc/autoload" "\
For some reason smartparens won't autoskip >'s, this hack does.

\(fn)" t nil)

(autoload '+cc-sp-point-is-template-p "../modules/lang/cc/autoload" "\
Return t if point is in the right place for C++ angle-brackets.

\(fn ID ACTION CONTEXT)" nil nil)

(autoload '+cc-sp-point-after-include-p "../modules/lang/cc/autoload" "\
Return t if point is in an #include.

\(fn ID ACTION CONTEXT)" nil nil)

(autoload '+cc-c-lineup-inclass "../modules/lang/cc/autoload" "\
Indent privacy keywords at same level as class properties.

\(fn LANGELEM)" nil nil)

(autoload '+cc|fontify-constants "../modules/lang/cc/autoload" "\
Better fontification for preprocessor constants

\(fn)" nil nil)

(autoload '+cc|irony-init-compile-options "../modules/lang/cc/autoload" "\
Initialize compiler options for irony-mode. It searches for the nearest
compilation database and initailizes it, otherwise falling back on
`+cc-default-compiler-options' and `+cc-default-include-paths'.

See https://github.com/Sarcasm/irony-mode#compilation-database for details on
compilation dbs.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "../modules/lang/javascript/autoload" "../modules/lang/javascript/autoload.el"
;;;;;;  (23321 60608 409013 990000))
;;; Generated autoloads from ../modules/lang/javascript/autoload.el

(autoload '+javascript-npm-conf "../modules/lang/javascript/autoload" "\
Retrieves an alist of this project's 'package.json'. If REFRESH-P is non-nil
ignore the cache.

\(fn &optional PROJECT-ROOT REFRESH-P)" nil nil)

(autoload '+javascript-npm-dep-p "../modules/lang/javascript/autoload" "\


\(fn PACKAGES &optional PROJECT-ROOT REFRESH-P)" nil nil)

(autoload '+javascript/repl "../modules/lang/javascript/autoload" "\
Open a Javascript REPL. Meaning either `skewer-repl', if any of the
skewer-*-mode's are enabled, or `nodejs-repl' otherwise.

\(fn)" t nil)

(autoload '+javascript/skewer-this-buffer "../modules/lang/javascript/autoload" "\
Toggle a globalized skewer-mode, attaching an external browser (once),
initiating an internal httpd server (once) and enabling the appropriate
skewer-mode for the current buffer.

Run this for any buffer you want to skewer.

\(fn)" t nil)

(autoload '+javascript/skewer-cleanup "../modules/lang/javascript/autoload" "\
Disable skewer-mode globally and disable the httpd server.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/lang/lisp/autoload" "../modules/lang/lisp/autoload.el"
;;;;;;  (23322 59524 665149 616000))
;;; Generated autoloads from ../modules/lang/lisp/autoload.el

(autoload '+emacs-lisp/repl "../modules/lang/lisp/autoload" "\
Open the Emacs Lisp REPL (`ielm').

\(fn)" t nil)

(autoload '+emacs-lisp-eval "../modules/lang/lisp/autoload" "\
Evaluate a region and print it to the echo area (if one line long), otherwise
to a pop up buffer.

\(fn BEG END)" nil nil)

;;;***

;;;### (autoloads nil "../modules/lang/python/autoload" "../modules/lang/python/autoload.el"
;;;;;;  (23314 53633 728863 765000))
;;; Generated autoloads from ../modules/lang/python/autoload.el

(autoload '+python/repl "../modules/lang/python/autoload" "\
Open the Python REPL.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/personal/basic/autoload/default"
;;;;;;  "../modules/personal/basic/autoload/default.el" (23321 60608
;;;;;;  413013 695000))
;;; Generated autoloads from ../modules/personal/basic/autoload/default.el

(autoload 'smart-beginning-of-line "../modules/personal/basic/autoload/default" "\
Move point to first non-whitespace character or beginning-of-line.

     Move point to the first non-whitespace character on this line.
     If point was already at that position, move point to beginning of line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/tools/gist/autoload/gist" "../modules/tools/gist/autoload/gist.el"
;;;;;;  (23321 60608 413013 695000))
;;; Generated autoloads from ../modules/tools/gist/autoload/gist.el

(autoload '+gist/open-current "../modules/tools/gist/autoload/gist" "\


\(fn)" t nil)

(autoload '+gist/kill-cache "../modules/tools/gist/autoload/gist" "\
Clears the gist cache. Necessary when a faulty cache causes gist.el to be
entirely unuseable.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/tools/impatient-mode/autoload"
;;;;;;  "../modules/tools/impatient-mode/autoload.el" (23321 60608
;;;;;;  413013 695000))
;;; Generated autoloads from ../modules/tools/impatient-mode/autoload.el

(autoload '+impatient-mode/toggle "../modules/tools/impatient-mode/autoload" "\
TODO

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/tools/make/autoload" "../modules/tools/make/autoload.el"
;;;;;;  (23321 60608 413013 695000))
;;; Generated autoloads from ../modules/tools/make/autoload.el

(autoload '+make/run "../modules/tools/make/autoload" "\
Run a make task in the current project.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/tools/neotree/autoload" "../modules/tools/neotree/autoload.el"
;;;;;;  (23321 60608 413013 695000))
;;; Generated autoloads from ../modules/tools/neotree/autoload.el

(autoload '+neotree/toggle "../modules/tools/neotree/autoload" "\
Toggle the neotree window.

\(fn)" t nil)

(autoload '+neotree/collapse-or-up "../modules/tools/neotree/autoload" "\
Collapse an expanded directory node or go to the parent node.

\(fn)" t nil)

(autoload '+neotree/collapse "../modules/tools/neotree/autoload" "\
Collapse a neotree node.

\(fn)" t nil)

(autoload '+neotree/expand-or-open "../modules/tools/neotree/autoload" "\
Expand or open a neotree node.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/tools/term/autoload" "../modules/tools/term/autoload.el"
;;;;;;  (23321 60608 413013 695000))
;;; Generated autoloads from ../modules/tools/term/autoload.el

(autoload '+term/open "../modules/tools/term/autoload" "\
Open a terminal buffer in the current window. If PROJECT-ROOT (C-u) is
non-nil, cd into the current project's root.

\(fn &optional PROJECT-ROOT)" t nil)

(autoload '+term/open-popup "../modules/tools/term/autoload" "\
Open a terminal popup window. If PROJECT-ROOT (C-u) is non-nil, cd into the
current project's root.

\(fn &optional PROJECT-ROOT)" t nil)

(autoload '+term/open-popup-in-project "../modules/tools/term/autoload" "\
Open a terminal popup window in the root of the current project.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../modules/tools/tmux/autoload/tmux" "../modules/tools/tmux/autoload/tmux.el"
;;;;;;  (23321 60608 413013 695000))
;;; Generated autoloads from ../modules/tools/tmux/autoload/tmux.el

(autoload '+tmux "../modules/tools/tmux/autoload/tmux" "\
Execute COMMAND in tmux

\(fn COMMAND &rest ARGS)" nil nil)

(autoload '+tmux/run "../modules/tools/tmux/autoload/tmux" "\
Run COMMAND in tmux. If NORETURN is non-nil, send the commands as keypresses
but do not execute them.

\(fn COMMAND &optional NORETURN)" t nil)

(autoload '+tmux/send-region "../modules/tools/tmux/autoload/tmux" "\
Send region to tmux.

\(fn BEG END &optional NORETURN)" t nil)

(autoload '+tmux/rerun "../modules/tools/tmux/autoload/tmux" "\
Rerun the last command executed by `+tmux' and `+tmux/run'.

\(fn)" t nil)

(autoload '+tmux/cd "../modules/tools/tmux/autoload/tmux" "\
Change the pwd of the currently active tmux pane to DIRECTORY (defaults to
`default-directory', or to `doom-project-root' with the universal argument).

\(fn &optional DIRECTORY)" t nil)

(autoload '+tmux/cd-to-here "../modules/tools/tmux/autoload/tmux" "\
cd into `default-directory' in tmux.

\(fn)" t nil)

(autoload '+tmux/cd-to-project "../modules/tools/tmux/autoload/tmux" "\
cd into `doom-project-root' in tmux.

\(fn)" t nil)

(autoload '+tmux-list-sessions "../modules/tools/tmux/autoload/tmux" "\


\(fn)" nil nil)

(autoload '+tmux-list-windows "../modules/tools/tmux/autoload/tmux" "\


\(fn &optional SESSION)" nil nil)

(autoload '+tmux-list-panes "../modules/tools/tmux/autoload/tmux" "\


\(fn &optional SESS-OR-WIN)" nil nil)

;;;***

;;;### (autoloads nil "../modules/tools/upload/autoload" "../modules/tools/upload/autoload.el"
;;;;;;  (23321 60608 413013 695000))
;;; Generated autoloads from ../modules/tools/upload/autoload.el

(autoload '+upload/local "../modules/tools/upload/autoload" "\
TODO

\(fn &optional FORCE-P)" t nil)

(autoload '+upload/remote-download "../modules/tools/upload/autoload" "\
TODO

\(fn)" t nil)

(autoload '+upload/diff "../modules/tools/upload/autoload" "\
TODO

\(fn)" t nil)

(autoload '+upload/browse "../modules/tools/upload/autoload" "\
TODO

\(fn)" t nil)

(autoload '+upload/check-remote "../modules/tools/upload/autoload" "\
TODO

\(fn)" t nil)

;;;***

(provide 'autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoloads.el ends here
