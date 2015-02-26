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

(provide 'setup-flymake)
