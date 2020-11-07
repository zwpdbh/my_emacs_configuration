
(when (maybe-require-package 'mermaid-mode)
  ;; (setq mermaid-mode-map
  ;;       (let ((map mermaid-mode-map))
  ;;         (define-key map (kbd "C-c C-c") nil)
  ;;         (define-key map (kbd "C-c C-f") nil)
  ;;         (define-key map (kbd "C-c C-b") nil)
  ;;         (define-key map (kbd "C-c C-r") nil)
  ;;         (define-key map (kbd "C-c C-o") nil)
  ;;         (define-key map (kbd "C-c C-d") nil)
  ;;         (define-key map (kbd "C-c C-d c") 'mermaid-compile)
  ;;         (define-key map (kbd "C-c C-d c") 'mermaid-compile)
  ;;         (define-key map (kbd "C-c C-d f") 'mermaid-compile-file)
  ;;         (define-key map (kbd "C-c C-d b") 'mermaid-compile-buffer)
  ;;         (define-key map (kbd "C-c C-d r") 'mermaid-compile-region)
  ;;         (define-key map (kbd "C-c C-d o") 'mermaid-open-browser)
  ;;         (define-key map (kbd "C-c C-d d") 'mermaid-open-doc)
  ;;         map))
  (add-hook 'mermaid-mode-hook
            '(lambda ()
               (define-key (current-local-map) (kbd "C-c C-c") 'mermaid-compile-buffer)
               (define-key (current-local-map) (kbd "C-c C-d o") 'mermaid-open-browser)
               (define-key (current-local-map) (kbd "C-c C-d d") 'mermaid-open-doc)))
  
  (when (maybe-require-package 'ob-mermaid)
    (setq ob-mermaid-cli-path "/usr/local/binmmdc")

    (add-hook 'org-mode-hook
              '(lambda ()
                 (add-to-list 'zw/org-babel-evaluate-whitelist "mermaid")
                 (add-to-list 'zw/org-babel-load-language-list '(mermaid . t))
                 (add-to-list 'org-structure-template-alist '("mermaid" . "src mermaid"))))))

(provide 'init-mermaid)