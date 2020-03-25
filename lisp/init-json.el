(use-package json-mode
  :defer t
  :ensure t
  :config
  (progn
    (add-hook 'json-mode-hook
              #'(lambda ()
                  (remove-hook 'before-save-hook #'clang-format-buffer-smart 'local)))))

(provide 'init-json)