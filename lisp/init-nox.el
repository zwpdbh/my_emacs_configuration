(add-to-list 'load-path
             "~/.emacs.d/site-lisp/posfram")
(require 'posframe)

(add-to-list 'load-path
             "~/.emacs.d/site-lisp/nox")
(require 'nox)

(dolist (each-mode zw/lsp-clients-set)
  (add-hook (intern (format "%s-hook" each-mode))
            #'(lambda ()
                (nox-ensure)
                (zw/customize-xref-key-bindings))))

(add-hook 'nox-managed-mode-hook
          '(lambda ()
             (when (nox-managed-p)
               (zw/set-company-backends-global))))

(provide 'init-nox)