;; install clang: =sudo apt install clang=
;; install clangd: https://clang.llvm.org/extra/clangd/Installation.html#installing-clangd

(use-package cquery
  :defer t
  :init
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
                (cquery//enable))))
  :ensure t)

;; support CMakeLists
(use-package cmake-mode
  :defer t
  :init 
  (add-hook 'cmake-mode-hook #'(lambda ()
                                 ;; (smartparens-mode +1)
                                 ))
  :ensure t)

(provide 'init-c-and-c++)