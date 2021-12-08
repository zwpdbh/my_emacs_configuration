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
                             :debounce 1 'any))

    ;; Optionally configure a function which returns the project root directory.
    ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
    (setq consult-project-root-function
          (lambda ()
            (when-let (project (project-current))
              (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
    ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
    ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

    ;; (setq consult-narrow-key "l")
    (global-set-key (kbd "C-x M-:") 'consult-complex-command)
    (global-set-key (kbd "C-x b") 'consult-buffer)
    ;; change from swiper to this because it is faster
    (global-set-key (kbd "C-s") 'consult-line)

    (global-set-key (kbd "C-r") 'consult-outline)
    (global-set-key (kbd "M-g M-g") 'consult-goto-line)))

;; (when (maybe-require-package 'consult-selectrum)
;;   (after-load 'selectrum
;;     (require 'consult-selectrum)))

(provide 'init-consult)
