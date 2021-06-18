;; flow-based autocomplete for emacs with https://github.com/aaronjensen/company-flow
;; need to install https://github.com/facebook/flow
;; Tern is a stand-alone code-analysis engine for JavaScript, need to install: sudo npm install -g tern.

(add-to-list 'interpreter-mode-alist (cons "node" 'javascript-mode))
(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\|ts\\)\\(\\.erb\\)?\\'" . javascript-mode))
(add-to-list 'interpreter-mode-alist '("node" . javascript-mode))

(after-load 'js
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'zw/counsel-etags-setup)

(provide 'init-javascript)
