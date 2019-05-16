(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)



(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)


(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Use my init file in org-mode to set other packages
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (gradle-mode meghanada org-mode prettier-js sly company-flx cmake-ide t: helm-gtags ivy-hydra jedi helm-cider helm aggressive-indent rainbow-delimiters cider base16-theme gruvbox-theme zenburn-theme yaml-mode xref-js2 which-key web-mode use-package try treemacs-projectile treemacs-magit treemacs-icons-dired treemacs-evil tide spacemacs-theme solarized-theme slime-company racket-mode paredit nlinum neotree material-theme leuven-theme json-mode js2-refactor indent-tools indent-guide htmlize flycheck-yamllint exec-path-from-shell esup ensime elpy elisp-slime-nav dirtree counsel company-tern autopair auto-package-update adaptive-wrap ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
(put 'downcase-region 'disabled nil)
