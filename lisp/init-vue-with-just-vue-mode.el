(when (maybe-require-package 'vue-mode)
  (setq mmm-submode-decoration-level 2)
  (set-face-background 'mmm-default-submode-face (face-attribute 'default :background))

  (add-hook 'vue-mode-hook
            '(lambda ()
               (zw/counsel-etags-setup))))

(provide 'init-vue-with-just-vue-mode)