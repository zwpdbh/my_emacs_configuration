(when (maybe-require-package 'lsp-mode)
  (require 'lsp-clients)
  (when (maybe-require-package 'helm-lsp))
  (when (maybe-require-package 'lsp-treemacs))
  (when (maybe-require-package 'company-lsp)
    (eval-after-load 'company-lsp
      '(progn
         (setq company-lsp-cache-candidates nil)
         (setq company-lsp-async t)
         (setq company-lsp-enable-recompletion t))))
  (when (maybe-require-package 'lsp-ui)
    ;; "lsp-ui" because C-h f lsp-ui-mode RET says that lsp-ui-mode is defined in lsp-ui.el.
    (eval-after-load 'lsp-ui
      '(progn
         ;; Establishing keybindings for lsp-ui-mode
         ;; Variable lsp-ui-mode-map is available only after lsp-ui.el or lsp-ui.elc is loaded.
         (setq lsp-ui-peek-force-fontify t)
         (setq lsp-ui-imenu-enable t)
         (setq lsp-ui-sideline-ignore-duplicate t)
         (setq lsp-ui-sideline-enable nil)
         (setq lsp-ui-doc-enable nil)
         (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
         (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))))
  
  (eval-after-load 'lsp-mode
    '(progn
       (setq lsp-message-project-root-warning t)
       ;; change nil to 't to enable logging of packets between emacs and the LS
       ;; this was invaluable for debugging communication with the MS Python Language Server
       ;; and comparing this with what vs.code is doing
       (setq lsp-print-io nil)
       (add-hook 'lsp-mode-hook
                 '(lambda ()
                    ;; Code run from x-mode-hook is for buffer-specific things which means
                    ;; run the code for every x buffer
                    #'lsp-ui-mode)))))

(provide 'init-lsp)
