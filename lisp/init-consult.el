(when (maybe-require-package 'consult)
  (after-load 'consult
    ;; Disable the automatic preview only for below commands, where the preview may be expensive due to file loading.
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-file consult--source-project-file consult--source-bookmark
     :preview-key (kbd "M-.")))
  
  ;; (setq consult-narrow-key "l")
  (global-set-key (kbd "C-x M-:") 'consult-complex-command)
  (global-set-key (kbd "C-x b") 'consult-buffer)
  ;; change from swiper to this because it is faster
  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key (kbd "C-r") 'consult-outline)
  (global-set-key (kbd "M-g M-g") 'consult-goto-line))

;; (when (maybe-require-package 'consult-selectrum)
;;   (after-load 'selectrum
;;     (require 'consult-selectrum)))

(provide 'init-consult)
