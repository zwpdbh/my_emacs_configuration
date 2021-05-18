(use-package racket-mode
  :defer t
  :init
  (cond
   ((string-equal system-type "windows-nt")
    (setq racket-program "c:/Program Files/Racket/Racket.exe"))
   ((string-equal system-type "gnu/linux")
    (setq racket-program "/usr/bin/racket"))
   ((string-equal system-type "darwin")
    (setq racket-program "/Applications/Racket_v7.0/bin/racket")))
  ;; set racket-mode associated with racket-mode
  (add-to-list 'auto-mode-alist '("\\.racket\\'" . racket-mode))
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
  :mode "\\.racket\\'"
  :ensure t)

(defun my-racket-mode-hook ()
  (set (make-local-variable 'company-backends)
       '((company-capf company-dabbrev-code)))
  (company-quickhelp-mode 0))

(add-hook 'racket-mode-hook '(lambda ()
                               (define-key racket-mode-map (kbd "C-c r") 'racket-run)
                               (my-racket-mode-hook)
                               #'racket-unicode-input-method-enable))

(add-hook 'racket-repl-mode-hook '(lambda ()
                                    (my-racket-mode-hook)
                                    #'racket-unicode-input-method-enable))


(provide 'init-racket)
