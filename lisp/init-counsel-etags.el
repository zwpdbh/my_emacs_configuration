;; (add-to-list 'load-path
;;              "~/.emacs.d/site-lisp/counsel-etags")
;; (require 'counsel-etags)
(use-package counsel
  :ensure t)

(when (executable-find "ctags")
  (when (maybe-require-package 'counsel-etags)
    ;; (require 'counsel-etags)

    ;; NOTICE!!!
    ;; Set Exuberant Ctags or Universal Ctags
    ;; On Windows, we install Universal Ctags by choco install universal-ctags    
    ;; On Ubuntu, we could install Universal Ctags by sudo snap install universal-ctags, then check it by universal-ctags --version.
    ;; Or install by sudo apt install universal-ctags, then check it by ctags --version.
    ;; Notice: There may be a ctags which is actually etags.
    ;; That etags (which is /usr/local/bin/ctags) is probably installed from Emacs (maybe by native compile?)
    ;; The problem is it may shadow the real ctags or universal-ctags.
    ;; So, the counsel-etags failed to find available ctags program
    (if *win64*
        (add-to-list 'exec-path "c:/ProgramData/chocolatey/bin/")
      (add-to-list 'exec-path "/snap/bin/"))
    
    (defun zw/counsel-etags-grep-at-point ()
      (interactive)
      (counsel-etags-grep (counsel-etags-tagname-at-point) nil nil t))

    (when (fboundp 'consult-ripgrep)
      (defun zw/consult-ripgrep-at-point (&optional dir initial)
        "zw modified: Search for regexp with rg in DIR with INITIAL input.

See `consult-grep' for more details."
        (interactive "P")
        (unless initial (setq initial (counsel-etags-tagname-at-point)))
        (unless dir (setq dir (zw/find-project-root-dir)))
        (consult--grep "Ripgrep" #'consult--ripgrep-builder dir initial))

      ;; try to use counsel-ripgrep
      (defun zw/counsel-etags-find-tag-api (tagname fuzzy current-file)
        "Find TAGNAME using FUZZY algorithm from CURRENT-FILE."
        (let* ((time (current-time))
               (dir (counsel-etags-tags-file-directory))
               (current-file (and current-file (file-local-name current-file))))
          (if dir (setq dir (file-local-name dir)))
          (when counsel-etags-debug
            (message "counsel-etags-find-tag-api called => tagname=%s fuzzy=%s dir%s current-file=%s"
                     tagname
                     fuzzy
                     dir
                     current-file))
          ;; Dir could be nil. User could use `counsel-etags-extra-tags-files' instead
          (cond
           ((and (not dir) (not counsel-etags-extra-tags-files))
            (message "Tags file is not ready yet."))
           ((not tagname)
            ;; OK, need use ivy-read to find candidate
            (ivy-read "Fuzz matching tags: "
                      `(lambda (s)
                         (counsel-etags-list-tag-function s ,current-file))
                      :history 'counsel-git-grep-history
                      :dynamic-collection t
                      :action `(lambda (e)
                                 (counsel-etags-open-file-api e ,dir))
                      :caller 'counsel-etags-find-tag
                      :keymap counsel-etags-find-tag-map))

           ((not (setq counsel-etags-find-tag-candidates
                       (counsel-etags-collect-cands tagname fuzzy current-file dir)))
            ;; OK, let's try grep the whole project if no tag is found yet
            (consult--grep "Ripgrep" #'consult--ripgrep-builder (zw/find-project-root-dir) tagname))
           (t
            ;; open the one selected candidate
            (counsel-etags-open-tag-cand tagname counsel-etags-find-tag-candidates time)))))
      
      (defun zw/counsel-etags-find-tag-at-point ()
        "Modified 'counsel-etags-find-tag-at-point to use consult--grep when failed to find tag"
        (interactive)
        (counsel-etags-tags-file-must-exist)
        (let* ((tagname (counsel-etags-tagname-at-point)))
          (cond
           (tagname
            (zw/counsel-etags-find-tag-api tagname nil buffer-file-name))
           (t
            (message "No tag at point")))))

      (after-load 'consult
        (consult-customize
         zw/consult-ripgrep-at-point
         zw/counsel-etags-find-tag-at-point
         :preview-key (kbd "M-."))))
    
    ;; adjust key-bindings for counsel-etags
    (defun zw/counsel-etags-key-bindings ()
      (interactive)      
      (if (fboundp 'zw/consult-ripgrep-at-point)
          (progn
            (define-key (current-local-map) (kbd "M-.") 'zw/counsel-etags-find-tag-at-point)
            (define-key (current-local-map) (kbd "M-/") 'zw/consult-ripgrep-at-point))
        (progn
          (define-key (current-local-map) (kbd "M-.") 'counsel-etags-find-tag-at-point)
          (define-key (current-local-map) (kbd "M-/") 'zw/counsel-etags-grep-at-point))))

    (setq zw/use-counsel-etags-modes '(yaml-mode
                                       json-mode))

    (dolist (each-mode zw/use-counsel-etags-modes)
      (add-hook (intern (format "%s-hook" each-mode))
                'zw/counsel-etags-setup)))


  ;; Ignore directories and files
  (after-load 'counsel-etags
    (setq tags-add-tables t)
    (setq imenu-create-index-function 'counsel-etags-imenu-default-create-index-function)

    ;; Don't ask before rereading the TAGS files if they have changed
    (setq tags-revert-without-query t)
    ;; Don't warn when TAGS files are large
    (setq large-file-warning-threshold nil)
    (setq counsel-etags-update-interval 30)
    
    ;; counsel-etags-ignore-directories does NOT support wildcast
    (add-to-list 'counsel-etags-ignore-directories "dist")
    (add-to-list 'counsel-etags-ignore-directories "build_clang")
    (add-to-list 'counsel-etags-ignore-directories "build")
    (add-to-list 'counsel-etags-ignore-directories "node_modules")
    
    ;; counsel-etags-ignore-filenames supports wildcast
    (add-to-list 'counsel-etags-ignore-filenames "TAGS")
    (add-to-list 'counsel-etags-ignore-filenames "package-lock.json")
    (add-to-list 'counsel-etags-ignore-filenames "#*")
    (add-to-list 'counsel-etags-ignore-filenames "#*.*#")
    (add-to-list 'counsel-etags-ignore-filenames "*.*#"))

  (add-hook 'after-init-hook (lambda () (require 'counsel-etags))))


(defun zw/counsel-etags-setup ()
  (interactive)
  (when (fboundp 'zw/counsel-etags-key-bindings)
    (zw/counsel-etags-key-bindings)
    (add-hook 'after-save-hook
              'counsel-etags-virtual-update-tags 'append 'local)))

(provide 'init-counsel-etags)
