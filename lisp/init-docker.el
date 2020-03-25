(use-package dockerfile-mode
  :defer t
  :ensure t
  :init 
  (setq dockerfile-mode-command "docker"))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode
  :after (dockerfile-mode)
  :defer t
  :ensure t)

(use-package docker-tramp
  ;; C-x C-f /docker:user@container:/path/to/file, where:
  ;; user is the user that you want to use
  ;; container is the id or name of the container 
  :defer t
  :ensure t
  :after (dockerfile-mode))

(use-package eshell-bookmark
  :defer t 
  :after eshell
  :config 
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

(provide 'init-docker)