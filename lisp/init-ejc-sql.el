(when (maybe-require-package 'ejc-sql)
  ;; Use a port other than 8080.
  (setq clomacs-httpd-default-port 8090)

  ;; set minibuffer completion
  (setq ejc-completion-system 'standard)

  ;; set table minor-mode
  ;; to maximize buffer performance, we could set it as 'ejc-result-mode
  ;; (setq ejc-result-table-impl 'orgtbl-mode)
  (setq ejc-result-table-impl 'ejc-result-mode)
  
  (require 'ejc-company)
  (add-hook 'ejc-sql-minor-mode-hook
            '(lambda ()
               (ejc-eldoc-setup)
               (setq-local company-backends
                           '(company-capf (ejc-company-backend company-dabbrev-code) company-keywords company-files) company-dabbrev)
               (company-mode t)))

  (add-hook 'ejc-sql-connected-hook
            (lambda ()
              (ejc-set-fetch-size 50)
              (ejc-set-max-rows 50)
              (ejc-set-show-too-many-rows-message t)
              (ejc-set-column-width-limit 25)
              (ejc-set-use-unicode t)))
  (add-hook 'sql-mode-hook
            'ejc-sql-mode))

(provide 'init-ejc-sql)