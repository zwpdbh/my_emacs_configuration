(when (maybe-require-package 'lsp-mode)
  (when (symbol-function 'helm)
    (maybe-require-package 'helm-lsp))
  (when (symbol-function 'treemacs)
    (maybe-require-package 'lsp-treemacs))
  (when (symbol-function 'company-mode)
    (maybe-require-package 'company-lsp))
  (maybe-require-package 'lsp-ui))


(after-load 'company-lsp
  (setq company-lsp-cache-candidates nil)
  (setq company-lsp-async t)
  (setq company-lsp-enable-recompletion t))

(after-load 'lsp-mode
  (setq lsp-message-project-root-warning t)
  ;; change nil to 't to enable logging of packets between emacs and the LS
  ;; this was invaluable for debugging communication with the MS Python Language Server
  ;; and comparing this with what vs.code is doing
  (setq lsp-print-io nil)
  (lsp-ui-mode t))

(after-load 'lsp-ui
  ;; Establishing keybindings for lsp-ui-mode
  ;; Variable lsp-ui-mode-map is available only after lsp-ui.el or lsp-ui.elc is loaded.
  (setq lsp-ui-peek-force-fontify t)
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-peek-fontify 'always)

  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))


(add-hook 'lsp-mode-hook
          '(lambda ()
             ;; Code run from x-mode-hook is for buffer-specific things which means
             ;; run the code for every x-mode buffer
             (setq-local company-backends (add-to-list 'company-backends
                                                       'company-lsp))))

(setq my-lsp-mode-set '(js-mode
                        python-mode
                        sh-mode
                        c-mode
                        c++-mode))

(defun zw/customize-lsp-key-bindings-for ()
  (interactive)
  (define-key (current-local-map) (kbd "M-.") 'lsp-ui-peek-find-definitions)
  (if (display-graphic-p)
      (define-key (current-local-map) (kbd "M-/") 'lsp-ui-peek-find-references)
    (define-key (current-local-map) (kbd "C-x .") 'lsp-ui-peek-find-references)))


(dolist (each-mode my-lsp-mode-set)
  (let ((each-mode-hook (intern (format "%s-hook" each-mode))))
    (add-hook each-mode-hook
              #'(lambda ()
                  (lsp)
                  (zw/customize-lsp-key-bindings-for)))))


(provide 'init-lsp)
