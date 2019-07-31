(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)



(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("org"   . "http://elpa.emacs-china.org/org/")))


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
 '(org-agenda-files
   (quote
    ("~/code/org/clojure.org" "~/code/org/job.org" "~/.emacs.d/myinit.org" "~/code/org/emacs.org")))
 '(package-selected-packages
   (quote
    (org-tempo ob-http spacemacs-theme docker-tramp docker-compose-mode dockerfile-mode treemacs-icons-dired treemacs-projectile treemacs-evil treemacs ox-gfm htmlize yaml-mode which-key use-package try smartparens sly rainbow-delimiters racket-mode paredit nlinum monokai-theme magit lsp-ui lsp-python-ms lispy json-mode indent-guide highlight-indentation helm-xref helm-projectile helm-cider gruvbox-theme geiser flycheck-yamllint expand-region exec-path-from-shell ess ensime dap-mode cquery company-statistics company-quickhelp company-lsp cmake-mode clang-format aggressive-indent adoc-mode adaptive-wrap))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
