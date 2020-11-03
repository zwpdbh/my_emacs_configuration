(when (maybe-require-package 'vue-mode)
  (set-face-background 'mmm-default-submode-face nil)
  
  (add-hook 'vue-mode-hook
            '(lambda ()
               ;; (zw/counsel-etags-setup)
               (lsp)
               (zw/lsp-ui-key-bindings))))

(provide 'init-web)