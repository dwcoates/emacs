(defconst dwcB-major-prefix "C-e"
  "dwc-binding prefix used as default for major modes. Major mode commands may be bound to keys outside of prefix (see dwcB-add-major-mode-map).")
(defconst dwcB-inter-buffer-prefix "C-w"
  "dwc-binding prefix used as default for inter-buffer navigation and editing.")


(defvar dwcB-global-map (let ((map (make-sparse-keymap))) (set-keymap-parent map global-map) map)
  "The dwc-bindings global map. Should contain inter- and general intra-buffer binds. Inherits from global-map.")

(defvar dwcB-inter-buffer-map (define-key dwcB-global-map (kbd dwcB-inter-buffer-prefix) (make-sparse-keymap))
  "dwcB keymap dedicated to inter-buffer editing and navigation. All binds under prefix key dwcB-inter-buffer-prefix")


(defvar dwcB-minor-mode-alist '())
(defvar dwcB-major-mode-alist '())

(define-minor-mode dwcB-mode
  "Organized alternative bindings to Emacs defaults."
  :lighter " dwcB"
  :global t
  (if dwcB-mode
      (progn
        (dwcB--set-global-map)                                              ; set global map
        (mapc 'dwcB-activate-minor-map (mapcar 'car dwcB-minor-mode-alist)) ; set minor maps
        (add-hook 'after-change-major-mode-hook 'dwcB-update-major-map)
        (dwcB-update-major-map)
        )
    (dwcB--reset-global-map)                                              ; unset global map
    (mapc 'dwcB-deactivate-minor-map (mapcar 'car dwcB-minor-mode-alist)) ; unset minor maps
    (remove-hook 'after-change-major-mode-hook 'dwcB-update-major-map)
    (dwcB-update-major-map)
    )
  )



(defun dwcB--reset-global-map ()
  "Set the current global map to Emacs default, global-map."
  (if (eq dwcB-global-map (current-global-map))
      (use-global-map global-map))
  )

(defun dwcB--set-global-map ()
  "Set the current global map to dwcB-global-map."
  (if (eq (current-global-map) global-map)
      (use-global-map dwcB-global-map))
  )


(defun dwcB-create-map (KEYMAP BASE &optional PARENT)
  "Create a new keymap composed of KEYMAP and BASE, and that inherits from PARENT. If PARENT is nil,
created map will not inherit any bindings."
  (let ((dwcB-keymap (or (append KEYMAP BASE) (make-sparse-keymap))))
    (set-keymap-parent dwcB-keymap PARENT)
    dwcB-keymap
    )
  )


(defun dwcB-add-minor-map (MINOR-MODE KEYMAP BASE &optional PARENT)
  "Add minor mode, MINOR-MODE's, dwcB keymap, KEYMAP, to be used iff the minor mode is active."
  (let ((keymap (cons MINOR-MODE (dwcB-create-map KEYMAP BASE PARENT))))
    (add-to-list 'dwcB-minor-mode-alist keymap)
    (dwcB-activate-minor-binding (car keymap))
    )
  )



(defun dwcB-activate-minor-map (MINOR-MODE)
  (let ((mode-map (assoc MINOR-MODE dwcB-minor-mode-alist)))
    (if (not (equal mode-map nil))
        (add-to-list 'minor-mode-overriding-map-alist mode-map)
      (message "No dwcB-minor-mode-map for %s; Must be first added with dwcB-add-minor-map"
              MINOR-MODE)
      ))
  )

(defun dwcB-deactivate-minor-map (MINOR-MODE)
  (let ((mode-map (assoc MINOR-MODE dwcB-minor-mode-alist)))
    (if (not (equal mode-map nil))
        (assq-delete-all (car mode-map) minor-mode-overriding-map-alist)
      (error "Minor mode %s has no dwcB binding."))
    )
  )


(defun dwcB-add-major-map (MAJOR-MODE KEYMAP BASE &optional PARENT)
  "Create and add a dwcB major mode keymap to the dwcB-major-mode-alist."
  (let ((mode-map (assoc MAJOR-MODE dwcB-major-mode-alist))
        (keymap (dwcB-create-map KEYMAP BASE PARENT)))
    (if (equal mode-map nil)
        (add-to-list
         'dwcB-major-mode-alist
         (cons MAJOR-MODE (cons (current-local-map) keymap))) ; remember major's default local-map
      (setf (cdr (cdr mode-map)) keymap))
    (dwcB-update-major-map)
    keymap
    )
  )

(defun dwcB-update-major-map ()
  "When dwcB-mode is on, switch to corresponding dwcB keymap for the current major mode."
    (let ((mode-map (assoc major-mode dwcB-major-mode-alist)))
      (if (not (equal mode-map nil))
          (if dwcB-mode
              (use-local-map (cdr (cdr mode-map)))  ; set to major-mode dwcB keymap
            (use-local-map (car (cdr mode-map)))    ; reset to major-mode original keymap
            ))
      )
    )

(require 'default-bindings)

(provide 'dwc-bindings)
