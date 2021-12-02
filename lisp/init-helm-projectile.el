;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(when (maybe-require-package 'projectile)
  (setq-default projectile-mode-line-prefix " Proj")
  (setq projectile-completion-system 'helm)
  (after-load 'projectile
    (global-set-key (kbd "C-c p") 'projectile-command-map))
  (when (maybe-require-package 'ibuffer-projectile)))


(add-hook 'after-init-hook '(lambda ()
                              (projectile-global-mode)))

;; ref: https://erickgnavar.github.io/emacs-config/#org0149359
(defun zw/project-edit-dir-locals ()
  "Edit .dir-locals.el file in project root."
  (interactive)
  (find-file (expand-file-name ".dir-locals.el" (my/project-root))))

(defun zw/project-edit-direnv ()
  "Edit .envrc file in project root."
  (interactive)
  (find-file (expand-file-name ".envrc" (my/project-root))))

(defun my/project-root ()
  "Return project root path."
  (cdr (project-current)))

(defun my/project-p ()
  (project-current))

(defun my/project-name ()
  "Get project name extracting latest part of project path."
  (if (my/project-p)
      (second (reverse (split-string (my/project-root) "/")))
    nil))

(defun zw/project-switch ()
  "Switch to a project and trigger switch action."
  (interactive)
  ;; make sure all the projects list is available to be used
  (project--ensure-read-project-list)
  (let* ((projects (mapcar 'car project--list))
         (choice (completing-read "Switch to project: " projects))
         (default-directory choice))
    ;; `default-directory' must be defined so `project.el' can know is in a new project
    (zw/project-switch-action)))

(defun zw/project-switch-action ()
  "Switch to a new perspective which name is project's name and open `helm-ls-git-ls'."
  (interactive)
  (persp-switch (my/project-name))
  (helm-ls-git-ls))

(defun zw/project-kill-buffers ()
  "Kill all the related buffers to the current project and delete its perspective as well."
  (interactive)
  (let* ((project-name (my/project-name))
         (project (project-current))
         (buffers-to-kill (project--buffers-to-kill project)))
    (when (yes-or-no-p (format "Kill %d buffers in %s?" (length buffers-to-kill) (my/project-root)))
      (mapc #'kill-buffer buffers-to-kill)
      (persp-kill project-name))))

(defun zw/project-open-new-project ()
  "Open a project for the first time and add it to `project.el' projects list."
  (interactive)
  (let* ((project-path-abs (read-directory-name "Enter project root: "))
         ;; we need to define `default-directory' to be able to get the new project when `project-current' is called
         (default-directory (replace-regexp-in-string (expand-file-name "~") "~" project-path-abs)))
    (project-remember-project (project-current))
    (zw/project-switch-action)))


(when (maybe-require-package 'helm-projectile)
  (after-load 'projectile
    (after-load 'helm
      (helm-projectile-on))))

(after-load 'helm-projectile
  (cond ((and (fboundp 'zw/advice-projectile-use-rg)
              (not *win64*))
         (global-set-key (kbd "C-c p s s") 'helm-projectile-rg))
        ((executable-find "ag")
         (global-set-key (kbd "C-c p s s") 'helm-projectile-ag))
        (t
         (global-set-key (kbd "C-c p s s") 'helm-projectile-grep))))

(provide 'init-helm-projectile)