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
      (after-load 'consult
         (consult-customize zw/consult-ripgrep-at-point :preview-key (kbd "M-."))))
    
    ;; adjust key-bindings for counsel-etags
    (defun zw/counsel-etags-key-bindings ()
      (interactive)
      (define-key (current-local-map) (kbd "M-.") 'counsel-etags-find-tag-at-point)
      (if (fboundp 'zw/consult-ripgrep-at-point)
          (define-key (current-local-map) (kbd "M-/") 'zw/consult-ripgrep-at-point)
        (define-key (current-local-map) (kbd "M-/") 'zw/counsel-etags-grep-at-point)))

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
