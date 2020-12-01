(require 'web-mode)
(define-derived-mode zw/vue-mode web-mode "zw/vue-web"
  "A major mode derived from web-mode, for editing .vue file with LSP support.")

;; Associate Major Mode by File Name Extension
(add-to-list 'auto-mode-alist '("\\.vue\\'" . zw/vue-mode))
(add-hook 'zw/vue-mode-hook
          '(lambda ()
             (zw/set-company-backends-for-web-mode)))

;;;; disable eglot for frontend development
;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '(zw/vue-mode "vls"))
;; (add-hook 'zw/vue-mode-hook
;;           '(lambda ()
;;              (eglot-ensure)))

(provide 'init-vue-with-eglot-plus-web-mode)
