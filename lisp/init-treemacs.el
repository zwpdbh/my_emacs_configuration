(use-package treemacs
  :defer t
  :ensure t
  :init
  (use-package lv
    :defer t
    :ensure t)
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
        treemacs-file-event-delay           5000
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        ;; indent guide
        ;; treemacs-indentation-string (propertize " | " 'face 'font-lock-comment-face)
        ;; treemacs-indentation-string         "|"
        treemacs-is-never-other-window      nil
        treemacs-no-png-images              nil
        treemacs-project-follow-cleanup     nil
        treemacs-file-follow-delay          nil
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      40
        treemacs-follow-mode                t
        treemacs-filewatch-mode             t
        treemacs-git-mode nil)
  :config
  (progn
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
  :bind
  (:map global-map
        ("C-x t t"   . treemacs)
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :defer t
;;   :after (treemacs evil) 
;;   :ensure t)

(use-package treemacs-projectile
  :defer t
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :defer t
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(provide 'init-treemacs)
