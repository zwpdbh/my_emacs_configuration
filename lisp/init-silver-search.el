(use-package ag
  :ensure t
  :defer t)

(use-package helm-ag
  :ensure t
  :defer t
  :commands (helm-projectile-ag))

(provide 'init-silver-search)
