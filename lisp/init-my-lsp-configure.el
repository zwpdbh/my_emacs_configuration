(setq zw/use-lsp 'nil)

(setq my-lsp-mode-set '(js-mode
                        python-mode
                        sh-mode))

(if zw/use-lsp
    (progn
      (require 'init-lsp)
      (require 'init-dap)
      (message "use lsp as language server protocol client"))
  (progn
    (require 'init-nox)
    (message "use nox as language server protocol client")))


(provide 'init-my-lsp-configure)