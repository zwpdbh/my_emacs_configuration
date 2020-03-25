(use-package yaml-mode
  :defer t
  :ensure t
  :config
  (progn
    (add-hook 
     'yaml-mode-hook 
     #'(lambda ()
         (setq yaml-indent-offset 2)
         ;; (smartparens-mode)
         (remove-hook 'before-save-hook #'clang-format-buffer-smart 'local)))))

(provide 'init-yaml)
