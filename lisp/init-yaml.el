(use-package yaml-mode
  :defer t
  :ensure t
  :config
  (progn
    (add-hook 'yaml-mode-hook 
              '(lambda ()
                 (setq yaml-indent-offset 2)
                 (remove-hook 'before-save-hook #'clang-format-buffer-smart 'local)))))

(after-load 'org
  ;; since yaml mode is not supported by org, create the command yourself
  (defun org-babel-execute:Dockerfile (body params) body)
  ;; notice: it is case sensitive
  (add-to-list 'org-structure-template-alist '("docker" . "src dockerfile"))
  (add-to-list 'org-structure-template-alist '("dockerfile" . "src dockerfile")))

(provide 'init-yaml)
