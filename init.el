(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Use my init file in org-mode to set other packages
;;(org-babel-load-file (expand-file-name "~/.emacs.d/myinit_for_windows.org"))
(cond
 ((string-equal system-type "windows-nt")
  (progn
    (org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))))
 ((string-equal system-type "darwin")
  (progn
    (org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))))
 ((string-equal system-type "gnu/linux")
  (progn
    (org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("028de01489a683696c64dcc2a01eaa663670d04202de3fce48ec3a5542bc2da5" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" default)))
 '(package-selected-packages
   (quote
    (typescript treemacs-magit treemacs-icons-dired auto-package-update dirtree slime-company json-mode xref-js2 js-refactor js2-mode flycheck-yamllint nlinum htmlize emacs-htmlize yaml-mode treemacs-projectile treemacs-evil treemacs ac-slime racket-mode slime macrostep elisp-slime-nav ensime which-key use-package try paredit material-theme leuven-theme indent-guide exec-path-from-shell counsel auto-complete ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
