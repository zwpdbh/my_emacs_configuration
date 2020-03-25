(use-package clang-format
  :defer t
  :ensure t
  :config
  (progn
    (defun clang-format-buffer-smart ()
      "Reformat buffer if .clang-format exists in the projectile root."
      (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
        (clang-format-buffer)))

    (dolist (each-hook '(c-mode-hook c++-mode-hook js-mode-hook))
      (add-hook each-hook 
                #'(lambda ()
                    (add-hook 'before-save-hook #'clang-format-buffer-smart nil 'local)))))) 

;; Do not forget to install clang-format: =sudo apt install clang-format=.

(provide 'init-clang-format)
