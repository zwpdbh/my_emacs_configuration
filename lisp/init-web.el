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
  ;; (set-face-attribute 'web-mode-html-tag-face nil :foreground "royalblue")
  ;; (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "powderblue")
  ;; (set-face-attribute 'web-mode-doctype-face nil :foreground "lightskyblue")
  
  ;; (setq web-mode-content-types-alist
  ;;       '(("vue" . "\\.vue\\'")))

  (add-hook 'web-mode-hook (lambda()
                             (zw/counsel-etags-setup))))

(use-package emmet-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'web-mode-hook  'emmet-mode)))

(use-package company-web
  :commands (web-mode)
  :defer t 
  :after (company web-mode)
  :ensure t)

(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


(provide 'init-web)