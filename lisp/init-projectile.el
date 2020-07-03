;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :init
  (setq-default projectile-mode-line-prefix " Proj")
  (setq projectile-completion-system 'helm)
  (setq projectile-globally-unignored-files '(".gitlab-ci.yml"))
  :ensure t
  :bind ("C-c p" . projectile-command-map))

(use-package ibuffer-projectile
  :ensure t
  :after (projectile))

(add-hook 'after-init-hook '(lambda ()
                              (projectile-global-mode)))

(provide 'init-projectile)
;;; init-projectile.el ends here
