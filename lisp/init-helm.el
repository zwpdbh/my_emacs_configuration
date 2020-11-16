;; (use-package helm-xref
;;   :init
;;   :ensure t
;;   :config
;;   (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-swoop
  :ensure t
  ;; :bind ("C-s" . helm-multi-swoop-projectile)
  :config
  (progn
    (setq helm-swoop-use-fuzzy-match t)
    (setq helm-swoop-use-line-number-face t)
    (setq helm-swoop-move-to-line-cycle t)
    (setq helm-swoop-split-with-multiple-windows t)
    (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)))

(use-package helm-projectile
  :after (projectile helm)
  :ensure t
  :config
  (helm-projectile-on))

(when (maybe-require-package 'helm-descbinds)
  (after-load 'helm
    (helm-descbinds-mode t)
    (global-unset-key (kbd "C-h b"))
    (global-set-key (kbd "C-h b") #'helm-descbinds)))

(use-package helm
  :diminish
  :ensure t
  :bind (("M-x" . helm-M-x))
  :config
  (progn
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    ;; C-x C-f runs the command counsel-find-file
    (global-unset-key (kbd "C-x C-f"))
    (global-set-key (kbd "C-x C-f") #'helm-find-files)
    ;; skip meaningless files, e.g. .DS_Store
    (setq helm-ff-skip-boring-files t)
    (delete '"\\.bbl$" helm-boring-file-regexp-list)    ;show .bbl file
    (add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")
    (add-to-list 'helm-boring-file-regexp-list ".*\.synctex\.gz$")
    (add-to-list 'helm-boring-file-regexp-list ".*\.url$")
    (add-to-list 'helm-boring-file-regexp-list "\\.dropbox$")
    (add-to-list 'helm-boring-file-regexp-list "Icon.*")
    (add-to-list 'helm-boring-file-regexp-list "#.*#$")
    (add-to-list 'helm-boring-file-regexp-list "\\.out$")
    
    (global-set-key (kbd "M-y") #'helm-show-kill-ring)
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-echo-input-in-header-line t

          ;; optional fuzzy matching for helm-M-x
          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-completion-in-region-fuzzy-match t

          ;; for helm-semantic-or-imenu
          helm-imenu-fuzzy-match t
          helm-semantic-fuzzy-match t)

    (defun spacemacs//helm-hide-minibuffer-maybe ()
      "Hide minibuffer in Helm session if we use the header line as input field."
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face
                       (let ((bg-color (face-background 'default nil)))
                         `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))
    (add-hook 'helm-minibuffer-set-up-hook
              'spacemacs//helm-hide-minibuffer-maybe)

    (setq helm-autoresize-max-height 45)
    (setq helm-autoresize-min-height 30)    
    (helm-autoresize-mode t)
    (helm-mode 1)))

(provide 'init-helm)
