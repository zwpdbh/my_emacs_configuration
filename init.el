(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)



(setq package-archives '(("gnu-qinghua"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa-qinghua" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("org-qinghua"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

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
 '(custom-safe-themes
   (quote
    ("8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" default)))
 '(org-agenda-files
   (quote
    ("~/code/my-site/content-org/software-engineering.org" "~/code/org/SICP/sicp-video.org" "~/code/org/siemens/authentication_and_authorization/with_webkey.org" "~/code/org/siemens/job.org" "~/code/org/clojure-programming/clojure.org" "~/code/my-site/content-org/mathematics.org" "~/.emacs.d/myinit.org")))
 '(package-selected-packages
   (quote
    (centaur-tabs leuven-theme monokai-Theme material-theme idle-highlight-in-visible-buffers-mode ggtags xref-js2 indium doom-themes company-tern js2-mode company-tabnine company-web highlight-indent-guides emmet-mode plantuml-mode graphviz-dot-mode electric-spacing org-download org-attach-screenshot auctex company-math pdf-tools dakrone-theme tangotango-theme color-theme-sanityinc-tomorrow ample-theme airline-themes base16-theme vue-mode org-tempo ob-http spacemacs-theme docker-tramp docker-compose-mode dockerfile-mode treemacs-icons-dired treemacs-projectile treemacs-evil treemacs ox-gfm htmlize yaml-mode which-key use-package try smartparens sly rainbow-delimiters racket-mode paredit nlinum monokai-theme magit lsp-ui lsp-python-ms lispy json-mode indent-guide highlight-indentation helm-xref helm-projectile helm-cider gruvbox-theme geiser flycheck-yamllint expand-region exec-path-from-shell ess ensime dap-mode cquery company-statistics company-quickhelp company-lsp cmake-mode clang-format aggressive-indent adoc-mode adaptive-wrap))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
