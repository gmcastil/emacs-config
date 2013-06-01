;; Configure ORG mode

;;;; Configure column wrapping in certain modes
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook
  '(lambda() (set-fill-column 80)))
