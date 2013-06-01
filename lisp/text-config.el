;;;; Customizations for editing text

;; Wrap lines at the edge of the buffer, but do not insert any
(add-hook 'text-mode-hook (lambda ()
			    (visual-line-mode)))

;; Enable spell-checking
