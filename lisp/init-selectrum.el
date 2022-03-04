(when (maybe-require-package 'selectrum)
  (add-hook 'after-init-hook
            '(lambda ()
               (selectrum-mode +1)))
  
  ;; https://github.com/raxod502/selectrum/wiki/Additional-Configuration#handle-complete-symbol-with-slime
  (after-load 'slime
    (advice-add 'slime-display-or-scroll-completions :around
                (defun my--slime-completion-in-region (_ completions start end)
                  (completion-in-region start end completions)))))



(provide 'init-selectrum)
