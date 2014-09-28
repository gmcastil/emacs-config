;;;; Configure themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Using a lambda function for this could be problematic, if I were reloading
;; my init file
(defun my-load-theme ()
  (load-theme 'solarized-dark t)
  ;; Once the theme is loaded, reconfigure the mode-line as needed
  (load "mode-line-config.el"))
(add-hook 'after-init-hook 'my-load-theme)

;;;; Clean the frame
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(mouse-wheel-mode t)
(set-scroll-bar-mode nil)
(set-fringe-mode 0)

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

;; Setting ring-bell-function to non-nil calls this function to ring the bell
(setq ring-bell-function
      (lambda ()
	(message "Beep")))

;;;; Keybinds and hooks
(global-set-key (kbd "C-x C-c") 'ask-before-closing)                            
(global-set-key [C-tab] 'other-window)
