
(when (maybe-require-package 'selectrum)
  (add-hook 'after-init-hook
            '(lambda ()
               (selectrum-mode +1))))

(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook
            '(lambda ()
               (marginalia-mode)
               ))
  (after-load 'selectrum
    ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
    (advice-add #'marginalia-cycle :after
                (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))))

(provide 'init-selectrum)