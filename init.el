;;;; init.el --- Emacs customizations

;; This is for my own Lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; This is for Lisp that isn't installed by package.el
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(require 'package)
(package-initialize)

(load "package-manager.el")
(load "ac-config.el")
(load "ui-config.el")
(load "python-config.el")
(load "erc-config.el")
(load "racket-config.el")
(load "system-config.el")
(load "latex-config.el")
(load "org-config.el")
(load "outline-config.el")
;;(load "text-config.el")

(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(global-flycheck-mode t)
(global-set-key [f7] 'find-file-in-repository)

(server-start)
