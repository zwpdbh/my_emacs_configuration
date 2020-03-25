(use-package yafolding
  :defer t
  :init (global-set-key (kbd "<f9>") 'yafolding-toggle-element)
  :commands (yafolding-toggle-element)
  :ensure t)

(provide 'init-folding)