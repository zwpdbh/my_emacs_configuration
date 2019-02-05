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
    (org-babel-load-file (expand-file-name "~/.emacs.d/myinit_for_windows.org"))))
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
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8d805143f2c71cfad5207155234089729bb742a1cb67b7f60357fdd952044315" "028de01489a683696c64dcc2a01eaa663670d04202de3fce48ec3a5542bc2da5" default)))
 '(package-selected-packages
   (quote
    (material-theme ace-window auto-complete indent-guide exec-path-from-shell use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
