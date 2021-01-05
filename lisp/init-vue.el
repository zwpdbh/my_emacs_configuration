;; use web-mode for vue mode 
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-hook 'web-mode-hook
          '(lambda ()
             ;; reindentation is not appropriate for dealing with .vue file.
             (setq-local electric-indent-inhibit t)
             (zw/counsel-etags-setup)))

(provide 'init-vue)