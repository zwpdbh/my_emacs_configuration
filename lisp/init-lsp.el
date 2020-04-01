(when (maybe-require-package 'lsp-mode)
  (require 'lsp-clients)
  (when (maybe-require-package 'helm-lsp))
  (when (maybe-require-package 'lsp-treemacs))
  (when (maybe-require-package 'company-lsp))
  (when (maybe-require-package 'lsp-ui)
    (add-hook 'lsp-mode-hook '(lambda ()
                                #'lsp-ui-mode
                                (setq lsp-message-project-root-warning t)
                                ;; change nil to 't to enable logging of packets between emacs and the LS
                                ;; this was invaluable for debugging communication with the MS Python Language Server
                                ;; and comparing this with what vs.code is doing
                                (setq lsp-print-io nil)
                                
                                (setq company-lsp-cache-candidates nil)
                                (setq company-lsp-async t)
                                (setq company-lsp-enable-recompletion t)

                                (setq lsp-ui-peek-force-fontify t)
                                (setq lsp-ui-imenu-enable t)
                                (setq lsp-ui-sideline-ignore-duplicate t)
                                (setq lsp-ui-sideline-enable nil)
                                (setq lsp-ui-doc-enable nil)
                                
                                (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
                                (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))))

(provide 'init-lsp)
