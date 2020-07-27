;; install clang: =sudo apt install clang=
;; install clangd: https://clang.llvm.org/extra/clangd/Installation.html#installing-clangd

(when (maybe-require-package 'cquery)
  (setq cquery-executable "/usr/local/bin/cquery")
  (setq cquery-extra-init-params '(:completion (:detailedLabel t)))
  
  (defun cquery//enable ()
    (condition-case nil
        (lsp)
      (user-error nil)))
  
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (ggtags-mode 1)
                (cquery//enable)
                ;; (smartparens-mode)
                ))))

(when (maybe-require-package 'company-c-headers)
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (setq-local company-backends
                           (add-to-list 'company-backends
                                        'company-c-headers)))))

(when (maybe-require-package 'cmake-mode)
  (add-hook 'cmake-mode-hook
            '(lambda ()
               (setq-local company-backends
                           (add-to-list 'company-backends
                                        'company-cmake)))))

(provide 'init-c-and-c++)