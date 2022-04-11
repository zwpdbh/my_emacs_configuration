(when (maybe-require-package 'consult)
  (when (fboundp 'consult-ripgrep)
    (defun zw/consult-ripgrep-at-point (&optional dir initial)
      "zw modified: Search for regexp with rg in DIR with INITIAL input. See `consult-grep' for more details."
      (interactive "P")
      (unless initial (setq initial (counsel-etags-tagname-at-point)))
      (unless dir (setq dir (funcall consult-project-root-function)))
      
      (consult--grep "Ripgrep" #'consult--ripgrep-builder dir initial)))
  
  

  (add-hook 'csharp-mode-hook '(lambda () (setq-local consult-project-root-function 'zw/find-project-root-dir-for-csharp)))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (setq-local consult-project-root-function 'zw/find-project-root-dir-for-emacs)))
  (add-hook 'elixir-mode-hook '(lambda () (setq-local consult-project-root-function 'zw/find-project-root-dir-prefer-gitignore)))
  

  ;; (setq consult-narrow-key "l")
  (global-set-key (kbd "C-x M-:") 'consult-complex-command)
  (global-set-key (kbd "C-x b") 'consult-buffer)
  ;; change from swiper to this because it is faster
  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key (kbd "C-r") 'consult-outline)
  (global-set-key (kbd "M-g M-g") 'consult-goto-line)
  (global-set-key (kbd "M-/") 'zw/consult-ripgrep-at-point)


  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  (after-load 'consult
    (consult-customize consult-theme
                       ;; Preview immediately on M-., on up/down after 0.5s, on any other key after 1s                       
                       :preview-key
                       (list (kbd "M-.")
                             :debounce 0.5 (kbd "<up>") (kbd "<down>")
                             :debounce 1 'any))
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     zw/consult-ripgrep-at-point
     consult-bookmark consult-buffer consult-recent-file
     consult-xref
     consult--source-bookmark
     :preview-key (kbd "M-."))))


(add-hook 'after-init-hook (lambda () (require 'consult)))

(provide 'init-consult)
