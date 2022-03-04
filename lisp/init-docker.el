(use-package dockerfile-mode
  :defer t
  :ensure t
  :init 
  (setq dockerfile-mode-command "docker"))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(after-load 'org
  ;; since yaml mode is not supported by org, create the command yourself
  (defun org-babel-execute:Dockerfile (body params) body)
  ;; notice: it is case sensitive
  (add-to-list 'org-structure-template-alist '("docker" . "src dockerfile"))
  (add-to-list 'org-structure-template-alist '("dockerfile" . "src dockerfile")))

(use-package docker-compose-mode
  :after (dockerfile-mode)
  :defer t
  :ensure t)


(provide 'init-docker)