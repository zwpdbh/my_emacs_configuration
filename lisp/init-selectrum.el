
(when (maybe-require-package 'selectrum)
  (add-hook 'after-init-hook
            '(lambda ()
               (selectrum-mode +1)))
  ;; (after-load 'selectrum
  ;;   (define-key selectrum-minibuffer-map (kbd "<tab>") 'selectrum-select-current-candidate))
  
  ;; https://github.com/raxod502/selectrum/wiki/Additional-Configuration#handle-complete-symbol-with-slime
  (after-load 'slime
    (advice-add 'slime-display-or-scroll-completions :around
                (defun my--slime-completion-in-region (_ completions start end)
                  (completion-in-region start end completions)))))

(when (maybe-require-package 'marginalia)  
  
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (define-key minibuffer-local-map (kbd "C-M-a") 'marginalia-cycle)
  
  (after-load 'selectrum
    (marginalia-mode)
    ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
    (advice-add #'marginalia-cycle :after
                (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))
  (set-face-attribute 'marginalia-documentation nil
                      :underline nil))



(provide 'init-selectrum)
