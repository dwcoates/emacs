;;; Code:
(require 'elpy)

(add-hook 'python-mode-hook
          (lambda ()
            (elpy-mode)
            (elpy-enable)))

(provide 'setup-python)
