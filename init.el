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
    ("423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "dd854be6626a4243375fd290fec71ed4befe90f1186eb5b485a9266011e15b29" "5e0b63e0373472b2e1cf1ebcc27058a683166ab544ef701a6e7f2a9f33a23726" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "c6b93ff250f8546c7ad0838534d46e616a374d5cb86663a9ad0807fd0aeb1d16" "1897b97f63e91a792e8540c06402f29d5edcbfb0aafd64b1b14270663d6868ee" "e47c0abe03e0484ddadf2ae57d32b0f29f0b2ddfe7ec810bd6d558765d9a6a6c" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" "947190b4f17f78c39b0ab1ea95b1e6097cc9202d55c73a702395fc817f899393" "075351c6aeaddd2343155cbcd4168da14f54284453b2f1c11d051b2687d6dc48" "0bff60fb779498e69ea705825a2ca1a5497a4fccef93bf3275705c2d27528f2f" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "bffb799032a7404b33e431e6a1c46dc0ca62f54fdd20744a35a57c3f78586646" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" default)))
 '(org-agenda-files
   (quote
    ("~/code/my-site/content-org/software-engineering.org" "~/code/org/SICP/sicp-video.org" "~/code/org/siemens/authentication_and_authorization/with_webkey.org" "~/code/org/clojure-programming/clojure.org" "~/code/my-site/content-org/mathematics.org" "~/.emacs.d/myinit.org")))
 '(package-selected-packages
   (quote
    (subatomic256-theme sexy-mono-chrome-theme zeno-theme zeon-theme almost-mono-themes almost-mono-theme parchment-theme sexy-monochrome-theme nord-theme eink-theme esup smart-mode-line spaceline-config sublime-themes centaur-tabs leuven-theme monokai-Theme material-theme idle-highlight-in-visible-buffers-mode ggtags xref-js2 indium doom-themes company-tern js2-mode company-tabnine company-web highlight-indent-guides emmet-mode plantuml-mode graphviz-dot-mode electric-spacing org-download org-attach-screenshot auctex company-math pdf-tools dakrone-theme tangotango-theme color-theme-sanityinc-tomorrow ample-theme airline-themes base16-theme vue-mode org-tempo ob-http spacemacs-theme docker-tramp docker-compose-mode dockerfile-mode treemacs-icons-dired treemacs-projectile treemacs-evil treemacs ox-gfm htmlize yaml-mode which-key use-package try smartparens sly rainbow-delimiters racket-mode paredit nlinum monokai-theme magit lsp-ui lsp-python-ms lispy json-mode indent-guide highlight-indentation helm-xref helm-projectile helm-cider gruvbox-theme geiser flycheck-yamllint expand-region exec-path-from-shell ess ensime dap-mode cquery company-statistics company-quickhelp company-lsp cmake-mode clang-format aggressive-indent adoc-mode adaptive-wrap)))
 '(plantuml-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
(put 'narrow-to-region 'disabled nil)
