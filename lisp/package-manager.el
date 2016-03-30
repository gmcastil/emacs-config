;; Package customizations

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/") t)

(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; Make more packages available with the package installer
(setq to-install
      '(ac-geiser geiser flymake-racket racket-mode magit yasnippet jedi
		  auto-complete autopair find-file-in-repository flycheck
		  exec-path-from-shell auctex erc-hl-nicks ac-math org-ac
		  outline-magic))

(mapc 'install-if-needed to-install)
