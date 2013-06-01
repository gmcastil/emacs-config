(autoload 'message-mode "message" "mode for e-mail" t)
(add-to-list 'auto-mode-alist
             '("\\.*mutt-*\\|.article\\|\\.followup" 
                . message-mode))


(add-hook 'message-mode-hook 
  (lambda()
    (auto-fill-mode t)    
    (setq fill-column 72)))    ; RFC 1855 for Usenet messages
