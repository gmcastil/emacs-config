;; Modeline configuration
;;
;; This is largely based on the mode-line configuration of Amit Patel.  I've
;; made a few changes to make it smaller and render better on my 13" laptop
;; display.
;;
;; http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;;
;; TODO: Don't include the pathname for buffers like ERC channels
;;
;; TODO: Do not flag buffers that are not generally saved with the red tag
;;       This would include things like REPL, shells, Dired, etc.  The list
;;       is probably rather long
;;
;; TODO: Fix the mode-line-read-only-face so that the top and bottom of the box
;;       match the rest of the mode-line
;;
;; TODO: Switching on mode-line-modified-face changes the thickness of the
;;       mode-line
;;

(setq-default mode-line-format
	      '(;; Position, including warning for 80 columns
		(:propertize "%4l:" face mode-line-position-face)
		(:eval (propertize "%3c" 'face
				   (if (>= (current-column) 80)
				       'mode-line-80col-face
				     'mode-line-position-face)))

		mode-line-client
		;; A few empty space
		"    "

		;; Show buffer status
		(:eval (cond ((equal major-mode 'erc-mode)
			      ("    "))
			     ((buffer-modified-p)
			      (propertize " ** "
					  'face 'mode-line-modified-face))
			     (buffer-read-only
			      (propertize " RO "
					  'face 'mode-line-read-only-face))
			     (t "    ")))

		;; More empty space
		"    "

		;; Directory and buffer or filename
		(:propertize (:eval (shorten-directory default-directory 20))
			     face mode-line-folder-face)
		(:propertize "%b"
			     face mode-line-filename-face)

		;; Narrowing, if appropriate
		" %n   "

		;; Mode indicator
		(:propertize mode-name
			     face mode-line-mode-face)))

;; Helper function to shorten paths
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)
   
(set-face-attribute 'mode-line nil
		    :foreground "gray60" :background "gray20"
		    :inverse-video nil
		    :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
		    :foreground "gray80" :background "gray40"
		    :inverse-video nil
		    :box '(:line-width 1 :color "gray40" :style nil))
(set-face-attribute 'mode-line-read-only-face nil
		    :inherit 'mode-line-face
		    :foreground "#4271ae"
		    :box '(:line-width 1 :color "#4271aen"))
(set-face-attribute 'mode-line-modified-face nil
		    :inherit 'mode-line-face
		    :foreground "#ffffff"
		    :background "#c82829"
		    :box '(:line-width 1 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
		    :inherit 'mode-line-face
		    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
		    :inherit 'mode-line-face
		    :foreground "#eab700"
		    :weight 'bold)
;; Make sure that the position font is monospaced, or the rest of the
;; mode-line will jitter all over the place
(set-face-attribute 'mode-line-position-face nil
		    :inherit 'mode-line-face
		    :family "Inconsolata" :height 120)
(set-face-attribute 'mode-line-mode-face nil
		    :inherit 'mode-line-face
		    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
		    :inherit 'mode-line-mode-face
		    :foreground "gray40"
		    :height 100)
(set-face-attribute 'mode-line-process-face nil
		    :inherit 'mode-line-face
		    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
		    :inherit 'mode-line-position-face
		    :foreground "black" :background "#eab700")
