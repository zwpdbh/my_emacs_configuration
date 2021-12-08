(when (maybe-require-package 'consult)
  (after-load 'consult
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-file consult--source-project-file consult--source-bookmark
     :preview-key (kbd "M-."))
    (consult-customize consult-theme
                       ;; Preview immediately on M-., on up/down after 0.5s, on any other key after 1s                       
                       :preview-key
                       (list (kbd "M-.")
                             :debounce 0.5 (kbd "<up>") (kbd "<down>")
                             :debounce 1 'any)))
  
  ;; (setq consult-narrow-key "l")
  (global-set-key (kbd "C-x M-:") 'consult-complex-command)
  (global-set-key (kbd "C-x b") 'consult-buffer)
  ;; change from swiper to this because it is faster
  (global-set-key (kbd "C-s") 'consult-line)
  
  ;; need to install sudo apt install ripgrep
  (when (executable-find "rg")
    (global-set-key (kbd "C-c p s s") 'consult-ripgrep))

  (global-set-key (kbd "C-r") 'consult-outline)
  (global-set-key (kbd "M-g M-g") 'consult-goto-line))

;; (when (maybe-require-package 'consult-selectrum)
;;   (after-load 'selectrum
;;     (require 'consult-selectrum)))

(provide 'init-consult)
