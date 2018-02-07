;;; completion/company/config.el -*- lexical-binding: t; -*-

(def-setting! :company-backend (modes &rest backends)
  "Prepends BACKENDS to `company-backends' in major MODES.

MODES should be one major-mode symbol or a list of them."
  `(progn
     ,@(cl-loop for mode in (doom-enlist (doom-unquote modes))
                for def-name = (intern (format "doom--init-company-%s" mode))
                collect
                `(defun ,def-name ()
                   (when (and (eq major-mode ',mode)
                              ,(not (eq backends '(nil))))
                     (require 'company)
                     (make-variable-buffer-local 'company-backends)
                     (dolist (backend (list ,@(reverse backends)))
                       (cl-pushnew backend company-backends :test #'equal))))
                collect `(add-hook! ,mode #',def-name))))


;;
;; Packages
;;

(def-package! company
  :commands (company-mode global-company-mode company-complete
             company-complete-common company-manual-begin company-grab-line)
  :config
  (setq company-idle-delay nil
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '(company-capf company-dabbrev company-ispell)
        company-transformers '(company-sort-by-occurrence))

  (after! yasnippet
    (nconc company-backends '(company-yasnippet)))

  (global-company-mode +1)

  ;; Minibuffer completion. Maybe delete this?
  (with-eval-after-load "company-autoloads"
    (setq company-begin-commands '(self-insert-command
                                   c-electric-lt-gt c-electric-colon
                                   completion-separator-self-insert-command)
          company-show-numbers t
          company-tooltip-align-annotations t)

    (defvar-local company-col-offset 0 "Horisontal tooltip offset.")
    (defvar-local company-row-offset 0 "Vertical tooltip offset.")

    (defun company--posn-col-row (posn)
      (let ((col (car (posn-col-row posn)))
            ;; `posn-col-row' doesn't work well with lines of different height.
            ;; `posn-actual-col-row' doesn't handle multiple-width characters.
            (row (cdr (posn-actual-col-row posn))))
        (when (and header-line-format (version< emacs-version "24.3.93.3"))
          ;; http://debbugs.gnu.org/18384
          (cl-decf row))
        (cons (+ col (window-hscroll) company-col-offset) (+ row company-row-offset))))

    (defun company-elisp-minibuffer (command &optional arg &rest ignored)
      "`company-mode' completion back-end for Emacs Lisp in the minibuffer."
      (interactive (list 'interactive))
      (case command
        ('prefix (and (minibufferp)
                      (case company-minibuffer-mode
                        ('execute-extended-command (company-grab-symbol))
                        (t (company-capf `prefix)))))
        ('candidates
         (case company-minibuffer-mode
           ('execute-extended-command (all-completions arg obarray 'commandp))
           (t nil)))))

    (defun minibuffer-company ()
      (unless company-mode
        (when (and global-company-mode (or (eq this-command #'execute-extended-command)
                                           (eq this-command #'eval-expression)))

          (setq-local company-minibuffer-mode this-command)
          (setq-local completion-at-point-functions
                      (list (if (fboundp 'elisp-completion-at-point)
                                #'elisp-completion-at-point
                              #'lisp-completion-at-point) t))
          (setq-local company-show-numbers nil)
          (setq-local company-backends '((company-elisp-minibuffer company-capf)))
          (setq-local company-tooltip-limit 8)
          (setq-local company-col-offset 1)
          (setq-local company-row-offset 1)
          (setq-local company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                                          company-preview-if-just-one-frontend))

          (company-mode 1)
          (when (eq this-command #'execute-extended-command)
            (company-complete)))))

  :diminish 'company-mode)


(def-package! company-statistics
  :after company
  :config
  (setq company-statistics-file (concat doom-cache-dir "company-stats-cache.el"))
  (quiet! (company-statistics-mode +1)))


(def-package! company-quickhelp
  :after company
  :config
  (company-quickhelp-mode +1)
  (setq company-quickhelp-max-lines 10
        company-idle-delay 2.0
        company-quickhelp-delay 0.25))

(def-package! company-dict
  :commands company-dict
  :config
  (defun +company|enable-project-dicts (mode &rest _)
    "Enable per-project dictionaries."
    (if (symbol-value mode)
        (cl-pushnew mode company-dict-minor-mode-list :test #'eq)
      (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list))))
  (add-hook 'doom-project-hook #'+company|enable-project-dicts))


;;
;; Autoloads
;;

(autoload 'company-capf "company-capf")
(autoload 'company-yasnippet "company-yasnippet")
(autoload 'company-dabbrev "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-etags "company-etags")
(autoload 'company-elisp "company-elisp")
(autoload 'company-files "company-files")
(autoload 'company-gtags "company-gtags")
(autoload 'company-ispell "company-ispell")

