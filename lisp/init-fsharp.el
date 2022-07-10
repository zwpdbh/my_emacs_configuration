;; need to set inferior-fsharp-program, see: https://github.com/fsharp/emacs-fsharp-mode
(when (maybe-require-package 'fsharp-mode)
  (when (maybe-require-package 'ob-fsharp)
    (after-load 'org
      (add-to-list 'zw/org-babel-load-language-list '(fsharp . t))
      (add-to-list 'org-structure-template-alist '("fsharp" . "src fsharp"))
      (add-to-list 'org-structure-template-alist '("fs" . "src fsharp")))))

(provide 'init-fsharp)
