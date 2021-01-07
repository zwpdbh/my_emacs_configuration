(when (maybe-require-package 'consult)
  ;; (setq consult-narrow-key "l")
  
  (after-load 'consult
    (global-set-key (kbd "C-x M-:") 'consult-complex-command)
    (global-set-key (kbd "C-x b") 'consult-buffer)
    (global-set-key (kbd "C-c p s s") 'consult-ripgrep))
  
  (when (maybe-require-package 'consult-selectrum)
    (after-load 'selectrum
      (require 'consult-selectrum))))

(provide 'init-consult)