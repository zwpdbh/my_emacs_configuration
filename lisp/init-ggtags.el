(use-package ggtags
  :defer t
  :ensure t
  :config
  (progn
    (add-hook 'ggtags-mode-hook
              (lambda ()
                (setq-local company-backends (add-to-list 'company-backends 'company-gtags))))))

(provide 'init-ggtags)
