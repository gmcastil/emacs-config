;; SLIME configurations

;; Emacs doesn't inherit the path from the shell, so we will set the appropriate directory
;; manually since I've installed CCL for just myself
(setenv "CCL_DEFAULT_DIRECTORY" "~/src/ccl/")

(setq inferior-lisp-program "~/bin/ccl64")
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(slime-setup '(slime-fancy))

;; Function to clear the SLIME buffer when invoked
(defun slime-clear-shell ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))

;; SLIME hooks and keybinds
(add-hook 'inferior-ess-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-p") 'comint-previous-input)
	    (local-set-key (kbd "C-n") 'comint-next-input)
	    ;; Overwrite the default behavior and make C-l clear the shell
	    (local-set-key (kbd "C-l") 'slime-clear-shell)))
