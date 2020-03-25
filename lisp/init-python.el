;; install [[https://dotnet.microsoft.com/download][dotnet-sdk]]
;; clone and install [[https://github.com/Microsoft/python-language-server][python-language-server]]

(use-package lsp-python-ms
  :defer t
  :init 
  (setq python-shell-interpreter "python3")
  (setq lsp-python-ms-dir
        (expand-file-name "~/python-language-server/output/bin/Release/"))
  (setq lsp-python-ms-executable
        "~/python-language-server/output/bin/Release/Microsoft.Python.LanguageServer")
  (add-hook 'python-mode-hook 'lsp-mode)
  ;; (add-hook 'python-mode-hook #'smartparens-mode)
  :ensure t)


(provide 'init-python)