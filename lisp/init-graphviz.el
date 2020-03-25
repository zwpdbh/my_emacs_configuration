(use-package graphviz-dot-mode
  :commands (graphviz-dot-mode)
  :ensure t
  :init
  (setq graphviz-dot-indent-width 4)
  ;; :config 
  ;; (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
  )

(provide 'init-graphviz)