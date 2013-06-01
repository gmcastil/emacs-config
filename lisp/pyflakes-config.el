;; Pyflakes config

;; I found this code snippet here:
;;
;; http://reinout.vanrees.org/weblog/2010/05/11/pep8-pyflakes-emacs.html
;;
;; It seems to work, but I should probably figure out how at some point

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; I have no idea what this does, but it seems to work
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "/usr/local/share/python/pyflakes" (list local-file))))
  
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pyflakes-init)))

