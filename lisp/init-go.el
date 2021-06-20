;; - go-mode with ob-go

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(when (maybe-require-package 'go-mode)
  (if (string-equal system-type "gnu/linux")
      (add-to-list 'exec-path "/usr/local/go/bin/")
    nil))

(when (maybe-require-package 'ob-go)
  (after-load 'org
    (add-to-list 'org-structure-template-alist '("go" . "src go"))
    (add-to-list 'zw/org-babel-load-language-list '(go . t))))


(defun zw/set-paredit-for-go ()
  (my/disable-paredit-spaces-before-paren)
  (paredit-mode t)
  (define-key go-mode-map (kbd "}") 'paredit-close-curly))

(add-hook 'go-mode-hook
          '(lambda ()             
             (setq-local tab-width 4)
             (zw/set-paredit-for-go)
             
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



;; - lsp should work with [[https://github.com/golang/tools/blob/master/gopls/README.md][gopls]]
;; - install it by ~go get golang.org/x/tools/gopls@latest~
;; Need to install gopls to use lsp
;; go get golang.org/x/tools/gopls@latest

;; ;; configure lsp related parameters
;; (defun zw/lsp-go-steup ()
;;   (setq lsp-gopls-use-placeholders t)
;;   (lsp-register-custom-settings
;;    '(("gopls.completeUnimported" t t)
;;      ("gopls.staticcheck" t t)))

;;   (add-hook 'before-save-hook #'lsp-organize-imports t t)
;;   (zw/customize-lsp-ui-key-bindings))

;; (add-hook 'go-mode-hook #'lsp-deferred)
;; (add-hook 'go-mode-hook '(lambda ()
;;                           #'zw/lsp-go-steup))

(provide 'init-go)
