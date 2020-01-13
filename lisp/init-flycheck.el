;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (when (maybe-require-package 'flycheck)
;;   (add-hook 'after-init-hook 'global-flycheck-mode)
;;   (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

;;   (when (maybe-require-package 'flycheck-color-mode-line)
;;     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(use-package flycheck
  :diminish
  :ensure t
  :config
  (progn
    (use-package flycheck-color-mode-line
      :ensure t
      :after (flycheck)
      :config
      (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
    (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
