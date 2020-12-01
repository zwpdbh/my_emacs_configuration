(when (maybe-require-package 'vue-mode)
  (setq mmm-submode-decoration-level 1)
  (set-face-background 'mmm-default-submode-face (face-attribute 'default :background))
  
  (add-hook 'vue-mode-hook
            '(lambda ()
               (zw/counsel-etags-setup)))
  (after-load 'vue-mode
    (add-hook 'vue-mode-hook 'add-node-modules-path)))

(provide 'init-vue-with-just-vue-mode)