(when (maybe-require-package 'ess)
  (cond (*is-a-mac*
         (setq inferior-ess-r-program "/usr/local/bin/R"))
        (*linux*
         (setq inferior-ess-r-program "/usr/local/bin/R"))
        (*win64*
         ;; you may also need to add execution path to windows system environment
         (setq exec-path (append exec-path '("C:/tools/R/R-3.6.2/bin")))
         (setq inferior-ess-r-program "C:/tools/R/R-3.6.2/bin/R.exe")))
  (when (maybe-require-package 'electric-spacing)
    (add-hook 'ess-mode-hook 
              '(lambda () 
                 (electric-spacing-mode)
                 (setq comint-input-ring-size 1000
                       ess-indent-level 4
                       ess-arg-function-offset 4
                       ess-else-offset 4
                       ess-continued-statement-offset 2
                       truncate-lines t
                       comment-column 4)))))

;; (use-package electric-spacing
;;   :after (ess)
;;   :defer t
;;   :ensure t)

;; (use-package ess
;;   :defer t
;;   :ensure t
;;   :init 
;;   (require 'ess-site)
;;   (cond ((eq system-type 'darwin)
;;          (setq inferior-ess-r-program "/usr/local/bin/R"))
;;         ((eq system-type 'gnu/linux)
;;          (setq inferior-ess-r-program "/usr/local/bin/R"))
;;         ((eq system-type 'windows-nt)
;;          ;; you may also need to add execution path to windows system environment
;;          (setq exec-path (append exec-path '("C:/tools/R/R-3.6.2/bin")))
;;          (setq inferior-ess-r-program "C:/tools/R/R-3.6.2/bin/R.exe")))
;;   (setq comint-input-ring-size 1000
;;         ess-indent-level 4
;;         ess-arg-function-offset 4
;;         ess-else-offset 4
;;         ess-continued-statement-offset 2
;;         truncate-lines t
;;         comment-column 4)		
;;   (add-hook 'ess-mode-hook 
;;             '(lambda () 
;;                (electric-spacing-mode))))


(provide 'init-R)