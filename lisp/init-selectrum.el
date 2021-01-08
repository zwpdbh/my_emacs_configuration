
(when (maybe-require-package 'selectrum)
  (add-hook 'after-init-hook
            '(lambda ()
               (selectrum-mode +1))))

(when (maybe-require-package 'marginalia)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (define-key minibuffer-local-map (kbd "C-M-a") 'marginalia-cycle)
  
  (after-load 'selectrum
    (marginalia-mode)
    ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
    (advice-add #'marginalia-cycle :after
                (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))))


(provide 'init-selectrum)
