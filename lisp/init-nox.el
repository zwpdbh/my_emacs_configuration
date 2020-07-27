(when (maybe-require-package 'nox)
  (maybe-require-package 'posframe)
  (dolist (hook (list
                 'js-mode-hook
                 ;; 'rust-mode-hook
                 'python-mode-hook
                 ;; 'ruby-mode-hook
                 ;; 'java-mode-hook
                 'sh-mode-hook
                 ;; 'php-mode-hook
                 'c-mode-common-hook
                 'c-mode-hook
                 ;; 'csharp-mode-hook
                 'c++-mode-hook))
    (add-hook hook '(lambda () (nox-ensure)))))

(provide 'init-nox)