;;;; init.el --- Emacs customizations
(setq gc-cons-threshold 400000000)

;; Set up package manager
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.milkbox.net/packages/")))
(package-initialize)

;; Install use-package, if it's not already installed, and then use use-package
;; for configuring all other packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; From use-package README
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
;; (setq use-package-verbose t)

;;; Now load the config
(require 'org)
(org-babel-load-file (concat user-emacs-directory "config.org"))

(setq gc-cons-threshold 800000)
