(require 'geiser)
(require 'ac-geiser)

(add-hook 'geiser-mode-hook
	  (lambda ()
	    (ac-geiser-setup)
	    (auto-complete-mode)))

(add-hook 'geiser-repl-mode-hook
	  (lambda ()
	    (ac-geiser-setup)
	    (auto-complete-mode)))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))
(setq geiser-active-implementations '(racket))
