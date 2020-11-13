;; Associate Major Mode by File Name Extension
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(add-hook 'web-mode-hook
          '(lambda ()
             (lsp)
             (zw/lsp-ui-key-bindings)))
(provide 'init-vue-with-lsp-plus-web-mode)