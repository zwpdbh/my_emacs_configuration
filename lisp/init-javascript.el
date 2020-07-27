;; flow-based autocomplete for emacs with https://github.com/aaronjensen/company-flow
;; need to install https://github.com/facebook/flow
;; Tern is a stand-alone code-analysis engine for JavaScript, need to install: sudo npm install -g tern.

(use-package js2-mode
  :defer t
  :init
  :ensure t
  :config
  (progn
    (setq-default js2-bounce-indent-p nil)
    (setq-default js-indent-level 2)
    ;; In Emacs >= 25, the following is an alias for js-indent-level anyway
    (setq-default js2-basic-offset 2)))

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))



;; (defun zw/use-tern-javascript ()
;;   "Use tern as javascript backend"
;;   (if (and (maybe-require-package 'company-tern)
;;            (maybe-require-package 'tern))
;;       (progn
;;         (setq tern-command (append tern-command '("--no-port-file")))
;;         ;; define how to find definitions and references
;;         (when (and (executable-find "ag")
;;                    (maybe-require-package 'xref-js2))
;;           (after-load 'js2-mode
;;             (define-key js2-mode-map (kbd "M-.") nil)
;;             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
;;           (add-hook 'js-mode-hook 
;;                     '(lambda ()
;;                        (setq-local company-backends (add-to-list 'company-backends 'company-tern))
;;                        (tern-mode)))))
;;     (zw/use-lsp-javascript)))

;; switch different backend
;; (if (version<= emacs-version "27.0")
;;     (zw/use-tern-javascript)
;;   (zw/use-lsp-javascript))



(after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/enable-js2-checks-if-flycheck-inactive ()
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)))
  (add-hook 'js2-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
  (js2-imenu-extras-setup))

(when (maybe-require-package 'add-node-modules-path)
  (after-load 'typescript-mode
    (add-hook 'typescript-mode-hook 'add-node-modules-path))
  (after-load 'js2-mode
    (add-hook 'js2-mode-hook 'add-node-modules-path)))


(provide 'init-javascript)
