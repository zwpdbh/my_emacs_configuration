(use-package lsp-mode
  :defer t
  :init
  (require 'lsp-clients)
  (setq lsp-message-project-root-warning t)
  ;; change nil to 't to enable logging of packets between emacs and the LS
  ;; this was invaluable for debugging communication with the MS Python Language Server
  ;; and comparing this with what vs.code is doing
  (setq lsp-print-io nil)
  :ensure t)

(use-package helm-lsp 
  :after (helm lsp)
  :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs 
  :after (lsp treemacs)
  :commands lsp-treemacs-errors-list)

(use-package company-lsp
  :after (company lsp)
  :init 
  (setq company-lsp-cache-candidates nil)
  (setq company-lsp-async t)
  (setq company-lsp-enable-recompletion t)
  :ensure t)

(use-package lsp-ui
  :after (lsp)
  :init 
  (setq lsp-ui-peek-force-fontify t)
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :ensure t
  :config
  (progn
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(provide 'init-lsp)
