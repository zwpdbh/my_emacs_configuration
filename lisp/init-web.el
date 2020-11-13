(use-package web-mode
  :defer t
  :ensure t
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-css-colorization t)

  (add-hook 'web-mode-hook
            '(lambda ()
               (zw/counsel-etags-setup))))

(use-package emmet-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'web-mode-hook  'emmet-mode)))

(when (maybe-require-package 'company-web)
  (add-hook 'web-mode-hook
            '(lambda ()
               (setq-local company-backends '(company-capf
                                              company-dabbrev-code
                                              company-web-html
                                              company-keywords
                                              company-files
                                              company-dabbrev)))))


(defun zw/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook 'zw/use-eslint-from-node-modules)


(provide 'init-web)