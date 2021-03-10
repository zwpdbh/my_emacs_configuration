;; use web-mode for vue mode 
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

;; define my dummy vue-mode to support vue-mode in org-babel
(define-derived-mode vue-mode web-mode ""
  (setq font-lock-defaults '(vue-mode-keywords t t)))

(add-hook 'web-mode-hook
          '(lambda ()
             ;; reindentation is not appropriate for dealing with .vue file.
             (setq-local electric-indent-inhibit t)
             (zw/counsel-etags-setup)))

(provide 'init-vue)