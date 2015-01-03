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

;; Tell Geiser which flavor of Scheme we are using
(setq geiser-active-implementations '(racket))

;; Need to specify the location of the racket REPL binary on Mac OS
(when (memq window-system '(mac ns))
  (setq geiser-racket-binary "/Applications/Racket v6.1.1/bin/racket"))
