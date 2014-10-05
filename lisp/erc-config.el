;; ERC configuration

(global-set-key (kbd "C-c e") 'join-erc)

(defun join-erc ()
  "Fire up ERC!"
  (interactive)
  (require 'erc)
  (require 'erc-hl-nicks)
  ;; Configs for erc-hl-nicks
  (erc-autojoin-mode t)
  (setq erc-autojoin-channels-alist
	'((".*\\.freenode.net" "#emacs" "#physics" "#python" "#math" "#R" "#lisp")))

  ;;(setq erc-nick "gmcastil" erc-pass "IRC_314*")

  (erc-track-mode t)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				  "324" "329" "332" "333" "343" "477"))

  ;; This sets the ERC prompt to include the channel name, which is an awesome thing
  ;; Code snippet from http://www.emacswiki.org/emacs/ErcConfiguration
  ;; Bug alert - (erc-default-target) returns nil when the prompt is first displayed.
  ;; Also, the code is ugly and needs fixing
  (setq erc-prompt (lambda ()
		     (if (and (boundp 'erc-default-recipients) (erc-default-target))
			 (erc-propertize (concat (erc-default-target) ">") 'read-only t
					 'rear-nonsticky t 'front-nonsticky t)
		       (erc-propertize (concat "ERC>") 'read-only t 'rear-nonsticky t
				       'front-nonsticky t))))

  ;; Don't show any of these notifications
  (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

  ;; Buttonize links in an intelligent way
  (setq erc-button-url-regexp
	"\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

  ;; If ERC buffer exists already, then switch to the last active ERC buffer
  (if (get-buffer "irc.freenode.net:6667")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC? ")
      (erc :server "irc.freenode.net" :port 6667))))
