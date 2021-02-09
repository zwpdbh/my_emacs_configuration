(defun zw/customize-theme (pkg fn)
  (if (featurep pkg)
      (apply #'fn)
    (after-load pkg
      (apply #'fn))))
(defun zw/customize-themes-for-company ()
  )



(provide 'init-themes-customization)