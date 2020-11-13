(when (maybe-require-package 'vue-mode)
  (setq mmm-submode-decoration-level 2)
  (set-face-background 'mmm-default-submode-face (face-attribute 'default :background))
  
  (add-hook 'vue-mode-hook
            '(lambda ()
               (require 'vue-html-mode)

               ;; (zw/counsel-etags-setup)               
               (lsp)
               (zw/lsp-ui-key-bindings))))

(provide 'init-web)