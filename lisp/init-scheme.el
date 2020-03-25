(use-package geiser
  :defer t
  :init
  ;; append exec-path to include chez scheme
  (cond ((eq system-type 'windows-nt)
         (setq exec-path (append exec-path '("c:/Program Files (x86)/Chez Scheme 9.5/bin/ti3nt"))))
        ((eq system-type 'darwin)
         (setq exec-path (append exec-path '("/usr/local/bin"))))
        ((eq system-type 'gnu/linux)
         (setq exec-path (append exec-path '("/usr/bin")))))
  ;; set Library directories
  (cond ((eq system-type 'windows-nt)
         (setenv "CHEZSCHEMELIBDIRS" "C:\\scheme\\lib;")
         (setenv "CHEZSCHEMELIBEXTS" ".sc;;.so;"))
        ((eq system-type 'darwin)
         ;; raven is the chez scheme package management tool
         (setenv "CHEZSCHEMELIBDIRS" "/usr/local/lib/raven")
         (setenv "CHEZSCHEMELIBEXTS" ".sc::.so:"))
        (t
         nil))

  (cond ((eq system-type 'darwin)
         (setq geiser-chez-binary "chez"))
        (t
         (setq geiser-chez-binary "chezscheme9.5")))

  ;; (setq geiser-active-implementations '(guile chez))
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))

  (setq geiser-mode-start-repl-p t)
  (add-to-list 'auto-mode-alist '("\\.scheme\\'" . scheme-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.racket\\'" . scheme-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))
  (add-hook 'scheme-mode-hook 'geiser-mode)
  :ensure t)


(provide 'init-scheme)
