
(when (maybe-require-package 'selectrum)
  (add-hook 'after-init-hook
            '(lambda ()
               (selectrum-mode +1)))
  (after-load 'selectrum
    (define-key selectrum-minibuffer-map (kbd "<tab>") 'selectrum-select-current-candidate))
  )

(when (maybe-require-package 'marginalia)
  (after-load 'marginalia
    (set-face-attribute 'marginalia-documentation nil
                        :underline nil))
  
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (define-key minibuffer-local-map (kbd "C-M-a") 'marginalia-cycle)
  
  (after-load 'selectrum
    (marginalia-mode)
    ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
    (advice-add #'marginalia-cycle :after
                (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))))


(provide 'init-selectrum)
