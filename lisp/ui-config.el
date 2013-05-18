;;;; Configure themes
;;
;; There are a couple of ways to do this.  The first would be to pass a lambda
;; expression as a function to run after init files are loaded
(add-hook 'after-init-hook
	  #'(lambda () (load-theme 'nzenburn t)))
;; The second way is to define a function explicitly and pass that to the
;; add-hook function like this:
;; (defun my-load-theme ()
;;   (load-theme 'nzenburn t)
;;   (add-hook 'after-init-hook 'my-load-theme))

;;(add-hook 'after-init-hook
;;	  #'(lambda () (load "mode-line-config.el")))

;;;; Set faces
(set-face-attribute 'default nil
		    :family "Inconsolata" :height 160)
(set-face-attribute 'font-lock-comment-face nil
		    :family "Inconsolata" :height 140)

;;;; Clean the frame
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode t)
(mouse-wheel-mode t)
(set-scroll-bar-mode nil)
(set-fringe-mode 0)

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
