(add-to-list 'load-path
             "~/.emacs.d/site-lisp/nox")

(require 'nox)
(setq my-eglot-mode-set '(js-mode
                          python-mode
                          sh-mode
                          c-mode-common
                          c-mode
                          c++-mode))

(dolist (each-mode my-eglot-mode-set)
  (add-hook (intern (format "%s-hook" each-mode))
            #'(lambda ()
                (eglot-ensure))))

(provide 'init-nox)