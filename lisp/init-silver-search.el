(use-package ag
  :ensure t
  :defer t)

;; available options see: https://github.com/emacsorphanage/helm-ag
(when (maybe-require-package 'helm-ag)
  (setq helm-ag-command-option "--hidden --ignore .git"
        helm-ag-insert-at-point 'symbol))

(provide 'init-silver-search)
