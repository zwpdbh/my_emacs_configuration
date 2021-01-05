;; use web-mode for vue mode 
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-hook 'web-mode-hook
          '(lambda ()
             (zw/counsel-etags-setup)))

(provide 'init-vue)