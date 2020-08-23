(add-to-list 'load-path
             "~/.emacs.d/site-lisp/posfram")
(require 'posframe)

(add-to-list 'load-path
             "~/.emacs.d/site-lisp/nox")
(require 'nox)

(dolist (each-mode my-lsp-mode-set)
  (add-hook (intern (format "%s-hook" each-mode))
            #'(lambda ()
                (nox-ensure)
                (zw/customize-xref-key-bindings))))


(provide 'init-nox)