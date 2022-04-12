(setq coding-system-for-write 'utf-8-unix)
;; ===== set buffer and shell 
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (progn
;;     (when (memq window-system '(mac ns x))
;;       (exec-path-from-shell-initialize))))

;; Show actual color from hex code or color string.
;; Run "M-x rainbow-mode" when needed.
(maybe-require-package 'rainbow-mode)

;; ===== try 
(use-package try
  :commands (try)
  :ensure t)

(use-package expand-region
  :ensure t
  :config
  (progn
    (global-set-key (kbd "M-=") 'er/expand-region)
    (global-set-key (kbd "M--") 'er/contract-region)))

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Instant scratch buffer for current mode
;; https://github.com/ieure/scratch-el
(add-to-list 'load-path
             "~/.emacs.d/site-lisp/scratch-el")
;; uses package "scratch"
(autoload 'scratch "scratch" nil t)
;; M-x scratch, Immediately create a scratch buffer with the same major mode as the current buffer’s.
;; C-u M-x scratch, Prompts for a major mode to create a scratch buffer with.

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


;; Show current buffer file name relative to a project containing .gitignore file 
(defun zw/buffer-file-name ()
  (let ((fullname (buffer-file-name (current-buffer))))
    (if (not fullname)
        (buffer-name)
      (let* ((splited (split-string fullname "/"))
             (filename-part (last splited))
             (except-last (butlast splited))
             (parent-folder (last except-last)))
        (if parent-folder
            (let ((project-path (zw/find-project-root-dir-prefer-gitignore)))
              (if project-path
                  (file-relative-name fullname project-path)
                (concat (car parent-folder) "/" (car filename-part))))
          fullname)))))

;; custom modeline to show file name
(setq-default mode-line-buffer-identification
              '(:eval (zw/buffer-file-name)))

(defun zw/get-buffer-file-name ()
  (interactive)
  (kill-new (zw/buffer-file-name)))

(provide 'init-convenient)
