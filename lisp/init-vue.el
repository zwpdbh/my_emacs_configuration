(when (maybe-require-package 'vue-mode)

  (add-hook 'vue-mode-hook
            '(lambda ()
               (zw/counsel-etags-setup)
               (set-face-background 'mmm-default-submode-face nil))))

(provide 'init-web)