(when (maybe-require-package 'consult)
  (after-load 'consult
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-buffer consult-recent-file
     consult-xref
     consult--source-bookmark     
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
    ;; (setq consult-project-root-function
    ;;       (lambda ()
    ;;         (when-let (project (project-current))
    ;;           (car (project-roots project)))))
    ;;;; 2. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-root-function #'projectile-project-root)
    ;;;; 3. vc.el (vc-root-dir)
    ;; (setq consult-project-root-function #'vc-root-dir)
    ;;;; 4. locate-dominating-file
    (defun zw/find-project-root-dir-for-csharp ()
      (interactive)
      (let ((sln-root (locate-dominating-file "."
                                              (lambda (parent) (directory-files parent nil "\\.sln"))))
            (git-root (locate-dominating-file "." ".git")))
        (cond (sln-root
               sln-root)
              (git-root
               git-root)
              (t
               (message "Couldn't decide project's root-dir")))))

    (defun zw/find-project-root-dir-for-emacs ()
      (interactive)
      (let* ((git-root (locate-dominating-file "." ".git"))
             (init-lisp-folder (concat git-root "lisp/")))
        (cond (init-lisp-folder
               init-lisp-folder)
              (git-root
               git-root)
              (t
               (message "Couldn't decide project's root-dir")))))


    (defun zw/find-project-root-dir-prefer-gitignore ()
      (interactive)
      (let ((gitignore-root (locate-dominating-file "."
                                              (lambda (parent) (directory-files parent nil "\\.gitignore"))))
            (git-root (locate-dominating-file "." ".git")))
        (cond (gitignore-root
               gitignore-root)
              (git-root
               git-root)
              (t
               (message "Couldn't decide project's root-dir")))))
    

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


    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)))


(add-hook 'after-init-hook (lambda () (require 'consult)))

(provide 'init-consult)
