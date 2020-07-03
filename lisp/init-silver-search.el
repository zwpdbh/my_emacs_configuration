(use-package ag
  :ensure t
  :defer t)

(use-package helm-ag
  ;; :init
  ;; (custom-set-variables
  ;;  '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  ;;  '(helm-ag-command-option "--all-text")
  ;;  '(helm-ag-insert-at-point 'symbol)
  ;;  '(helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'")))
  :ensure t
  :defer t
  :commands (helm-projectile-ag))

(provide 'init-silver-search)
