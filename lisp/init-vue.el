(when (maybe-require-package 'vue-mode)
  (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-submode-decoration-level 2)
  (set-face-background 'mmm-default-submode-face (face-attribute 'default :background))

  (add-hook 'vue-mode-hook
            '(lambda ()
               (setq syntax-ppss-table nil)
               (require 'vue-html-mode)
               ;; (zw/counsel-etags-setup)               
               (lsp)
               (zw/lsp-ui-key-bindings))))

(provide 'init-web)