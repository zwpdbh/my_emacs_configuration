(when (maybe-require-package 'graphviz-dot-mode)
  (setq graphviz-dot-indent-width 2)
  (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

  (add-hook 'graphviz-dot-mode-hook
            #'(lambda ()
               (define-key (current-local-map) (kbd "C-c C-c") 'graphviz-dot-preview)
               (define-key (current-local-map) (kbd "C-c C-p") 'compile)))

  (after-load 'org
    (require 'ob-dot)
    (add-to-list 'zw/org-babel-evaluate-whitelist "dot")
    (add-to-list 'zw/org-babel-load-language-list '(dot . t))
    (add-to-list 'org-structure-template-alist '("dot" . "src dot :cmdline -Kdot -Tpng :file tmp.png"))
    
    ;; set the major-mode for edit babel dot src block 
    (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))))

(provide 'init-graphviz)
