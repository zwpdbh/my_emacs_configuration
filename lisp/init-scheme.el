(when (maybe-require-package 'geiser)
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
        (*win64*
         (setq geiser-chez-binary "scheme.exe"))
        (t
         (setq geiser-chez-binary "chezscheme9.5")))

  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile chez))
  (setq geiser-mode-start-repl-p t)
  
  ;; the default extention for scheme is .scm, .racket and .rkt are associated with racket-mode instead of scheme-mode
  (add-to-list 'auto-mode-alist '("\\.scheme\\'" . scheme-mode))

  (add-hook 'scheme-mode-hook 'geiser-mode)
  ;; expression in after-load execute only once when the package loaded
  (after-load 'geiser-mode
              (define-key geiser-mode-map (kbd "C-c C-c") 'geiser-eval-definition))
  ;; expression in mode-hook executed very time when buffer opened
  (add-hook 'geiser-mode-hook
            '(lambda ()
              (setq-local company-backends '((company-capf company-dabbrev-code geiser-company-backend) company-keywords company-files company-dabbrev)))))


(provide 'init-scheme)
