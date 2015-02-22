; Dodge Coates

;; key chords  **************** DOESNT WORK **********************
;;(require 'key-chord)
;;(key-chord-mode 1)

(setq default-directory "/home/dodge/")

(cd "./.emacs.d") ; Temporarily exists so I can setup emacs a bit faster. 

;; set path to dependencies
(setq settings-dir 
      (expand-file-name "settings" user-emacs-directory))

;; set up load path
(add-to-list 'load-path settings-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; set up appearance early
(setq appearance
      (expand-file-name "appearance.el" settings-dir))
(require 'appearance)


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(require 'setup-package)
;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(magit
     paredit
     move-text
     gist
     htmlize
     visual-regexp
     markdown-mode
     fill-column-indicator
     flycheck
     flycheck-pos-tip
     flycheck-clojure
     flx
     flx-ido
     dired-details
     css-eldoc
     yasnippet
     smartparens
     ido-vertical-mode
     ido-at-point
     simple-httpd
     guide-key
     nodejs-repl
     restclient
     highlight-escape-sequences
     whitespace-cleanup-mode
     elisp-slime-nav
     git-commit-mode
     gitconfig-mode
     dockerfile-mode
     gitignore-mode
     clojure-mode
     groovy-mode
     prodigy
     cider
     )))

;;(condition-case nil
;;    (init--install-packages)
;;  (error
;;   (package-refresh-contents)
;;   (init--install-packages)))

(require 'sane-defaults)


;; guide-key, displays possible key binding completions
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-yasnippet)

(require 'setup-helm)
;(require 'ggtags)
;(add-hook 'c-mode-common-hook
;          (lambda ()
;            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;              (ggtags-mode 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; COMPANY ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(company-mode 1)
;;(eval-after-load 'company
;;  '(add-to-list 'company-backends 'company-irony))
;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
;;(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; AUTO-COMPLETE ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start auto-complete with emacs
(require 'auto-complete)
;; do default config for auto-completion
(require 'auto-complete-config)
(ac-config-default)
;; initialized auto-complete-c-headers with hook
(defun my:ac-c-header-init ()
  (require  'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; FLYMAKE ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake-google
(defun my:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   ;; cpplint executable can be found at:
   ;; http://google-styleguide.googlecode.com/svn/trunk/cpplint/cpplint.py
   '(flymake-google-cpplint-command "/usr/local/bin/cpplint.py"))
  (flymake-google-cpplint-load))
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)
;;start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; SEMANTIC ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on Semantic
(semantic-mode 1)
;; adds semantic as a suggestion backend to auto-complete
(defun my:add-semantic-to-autocomplete ()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
;; turn on ede mode
(global-ede-mode 1)
;; create a project for 'demo' program
;; you can use the system-include-path for setting up the system header file locations.
(ede-cpp-root-project "my project" :file "~/demo/src/main.cpp"
		      :include-path '("/../inc"))
;; prompts semantic to parse open buffers in its spare time
(global-semantic-idle-scheduler-mode 1)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; JDEE ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JDEE initialization            *** COMMENTED UNTIL JDEE SET UP AGAIN ***
;;(add-to-list 'load-path "~/.emacs.d/jdee-2.4.1/list")
;;(load "jde")



(require 'key-bindings)

;;(add-to-list 'load-path (expand-file-name "/home/dodge/.emacs.d/elpa/emacs-xkcd-1.0/emacs-xkcd.el"))
;;(require 'xkcd-mode)
;;(global-set-key (kbd "C-x C-k r") 'xkcd-rand)
;;(global-set-key (kbd "C-x C-k l") 'xkcd-latest)

;; conclude init by setting up specifics for the current user
;;(when (file-exists-p user-settings-dir)
;;  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
