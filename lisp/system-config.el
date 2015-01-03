(require 'exec-path-from-shell)

;; If we're on Mac OS, import all user environment variables
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
