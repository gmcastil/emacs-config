;; Configure load path
;; This is for my own Lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; This is for Lisp that isn't installed by package.el
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Emacs 24 calls 'package-initialize to load all installed packages, but
;; only after the entire init file is loaded.
(load "ui-config.el")
(load "package-manager.el")
;; (load "erc-config.el")
(load "slime-config.el")
;; (load "python-config.el")
(load "mode-line-config.el")
;; (load "pyflakes-config.el")
