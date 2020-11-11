(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))

(setq sql-connection-alist
      '((local-dsconsole (sql-product 'postgres)
                         (sql-port 5432)
                         (sql-server "localhost")
                         (sql-user "atlas")
                         ;; (sql-password "atlas")
                         (sql-database "dsconsole"))
        (server2 (sql-product 'postgres)
                 (sql-port 5432)
                 (sql-server "localhost")
                 (sql-user "user")
                 (sql-password "password")
                 (sql-database "db2"))))


(defun zw/sql-connect-dsconsole ()
  (interactive)
  (my-sql-connect 'postgres 'local-dsconsole))

(defun zw/sql-connect-server2 ()
  (interactive)
  (my-sql-connect 'postgres 'server2))

(defun my-sql-connect (product connection)
  ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;; you call the function
  (setq sql-product product)
  (sql-connect connection))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(provide 'init-sql)