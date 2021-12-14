;; flow-based autocomplete for emacs with https://github.com/aaronjensen/company-flow
;; need to install https://github.com/facebook/flow
;; Tern is a stand-alone code-analysis engine for JavaScript, need to install: sudo npm install -g tern.

(add-to-list 'interpreter-mode-alist (cons "node" 'javascript-mode))
(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\|ts\\)\\(\\.erb\\)?\\'" . javascript-mode))
(add-to-list 'interpreter-mode-alist '("node" . javascript-mode))

(after-load 'js
  (setq js-indent-level 2))

(when (maybe-require-package 'typescript-mode)
  (after-load 'org
              ;; since yaml mode is not supported by org, create the command yourself
              (defun org-babel-execute:typescript (body params) body)

              (add-to-list 'org-structure-template-alist '("typescript" . "src typescript"))
              (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))))

(add-hook 'js-mode-hook 'zw/counsel-etags-setup)
(add-hook 'js-mode-hook
          (lambda ()
            (when (string= (file-name-extension buffer-file-name) "ts")
              (setq-local js-indent-level 4))))

(provide 'init-javascript)
