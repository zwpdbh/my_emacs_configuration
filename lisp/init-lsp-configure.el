(setq zw/use-lsp 'nil)

;; (setq zw/lsp-clients-set '(js-mode
;;                            python-mode
;;                            sh-mode))
(setq zw/lsp-clients-set 'nil)

(if zw/use-lsp
    (progn
      (require 'init-lsp)
      (require 'init-dap)
      (message "use lsp as language server protocol client"))
  (progn
    (require 'init-nox)
    (message "use nox as language server protocol client")))


(provide 'init-lsp-configure)