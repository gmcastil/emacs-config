;;;; Configure themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;;; Clean the frame
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(mouse-wheel-mode t)
(set-scroll-bar-mode nil)
(set-fringe-mode 0)

;; Setting ring-bell-function to non-nil calls this function to ring the bell
(setq ring-bell-function
      (lambda ()
	(message "Beep")))

;;;; Some UI specific packages
(require 'ido)
(ido-mode t)

;;;; Set faces
(set-face-attribute 'default nil
		    :family "Anonymous Pro" :height 120)
(set-face-attribute 'font-lock-comment-face nil
		    :family "Anonymous Pro" :height 110)

;;;; UI Functions

;; Rewrite function to ask before closing Emacs
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Cancelled exit")))

(defun set-auto-fill-mode-on ()
  "Turns on auto-fill-mode and sets the columns to fill at"
  (setq fill-column 79)
  (turn-on-auto-fill))

(defun my-load-theme ()
  "Load a custom theme and then reset the mode-line"
  (load-theme 'solarized-dark t)
  (load "mode-line-config.el"))

;;;; Keybinds and hooks
(global-set-key (kbd "C-x C-c") 'ask-before-closing)
(global-set-key [C-tab] 'other-window)

(add-hook 'text-mode-hook 'set-auto-fill-mode-on)
(add-hook 'after-init-hook 'my-load-theme)
