;;;; Python mode settings
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

;;;; Jedi settings
(require 'jedi)
;; Need to run "pip install --user jedi" and "pip install --user epc" to get the
;; Python side of the library to work right
(setq jedi:server-command
      '("python2" "/home/castillo/.emacs.d/elpa/jedi-20140321.1323/jediepcserver.py"))

(add-hook 'python-mode-hook
	  (lambda ()
	    (jedi:setup)
	    (jedi:ac-setup)
	    (local-set-key "\C-cd" 'jedi:show-doc)
	    (local-set-key (kbd "M-SPC") 'jedi:complete)
	    (local-set-key (kbd "M-.") 'jedi:goto-definition)))

(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))

(defun flymake-activate ()
  "Activates flymake when a real buffer and you have write access"
  (if (and
       (buffer-file-name)
       (file-writable-p buffer-file-name))
      (progn
	(flymake-mode t)
	;; This is necessary since there are no flymake-mode hooks
	(local-set-key (kbd "C-c n") 'flymake-goto-next-error)
	(local-set-key (kbd "C-c p") 'flymake-goto-prev-error))))

(defun ca-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'ca-flymake-show-help)

(add-to-list 'flymake-allowed-file-name-masks
	     '("\\.py\\'" flymake-python-init))

(add-hook 'python-mode-hook 'flymake-activate)
(add-hook 'python-mode-hook 'auto-complete-mode)
