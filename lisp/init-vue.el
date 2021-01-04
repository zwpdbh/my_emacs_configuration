;; ;; vue-mode 
;; (when (maybe-require-package 'vue-mode)
;;   (add-hook 'vue-mode-hook
;;             '(lambda ()
;;                (zw/counsel-etags-setup)
;;                (setq vue-html-tab-width 2)))
;;   (after-load 'vue-mode
;;     (add-hook 'vue-mode-hook 'add-node-modules-path)))

;; use web-mode for vue mode 
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-hook 'web-mode-hook
          '(lambda ()
             (zw/counsel-etags-setup)))

(provide 'init-vue)