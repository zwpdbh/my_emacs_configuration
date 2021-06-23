;; - go-mode with ob-go

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(when (maybe-require-package 'go-mode)
  (cond ((string-equal system-type "gnu/linux")
         (add-to-list 'exec-path "/usr/local/go/bin/")
         (add-to-list 'exec-path "/usr/bin/"))
        (t
         nil))

  ;; ref: https://wiki.crdb.io/wiki/spaces/CRDB/pages/73105658/Ben+s+Go+Emacs+Setup
  (maybe-require-package 'gotest))

(when (maybe-require-package 'ob-go)
  (after-load 'org
    (add-to-list 'org-structure-template-alist '("go" . "src go"))
    (add-to-list 'zw/org-babel-load-language-list '(go . t))))


(after-load 'go-mode
  ;; disable annoying *Gofmt Errors* buffer
  (setq gofmt-show-errors nil)
  (define-key go-mode-map (kbd "C-c C-c") #'go-run))

(add-hook 'go-mode-hook
          '(lambda ()             
             (setq-local tab-width 4)
             
             (flycheck-mode t)             
             (when (featurep 'flycheck)
               (define-key (current-local-map) (kbd "C-c C-n") 'flycheck-next-error)
               (define-key (current-local-map) (kbd "C-c C-p") 'flycheck-previous-error))
             
             (add-hook 'before-save-hook 'gofmt-before-save nil 'local)
             ;; Godef, lets you quickly jump around the code
             ;; Install it by: go get github.com/rogpeppe/godef
             (if (locate-file "godef" exec-path)
                 (progn
                   (define-key go-mode-map (kbd "M-.") 'godef-jump)
                   (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
                   (define-key go-mode-map (kbd "M-/") 'zw/counsel-etags-grep-at-point))
               (progn
                 (define-key go-mode-map (kbd "M-.") 'counsel-etags-find-tag-at-point)
                 (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
                 (define-key go-mode-map (kbd "M-/") 'zw/counsel-etags-grep-at-point)))))


(provide 'init-go)
