(when (maybe-require-package 'dashboard)
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-items '((recents . 10)
                          (projects . 5))))

(after-load 'dashboard
  (set-face-attribute 'dashboard-items-face nil
                      :weight 'normal))


(provide 'init-dashboard)
