;;; init-reveal --- see: https://github.com/yjwen/org-reveal

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "file:///~/.emacs.d/reveal.js")
  (setq org-reveal-mathjax t))


(provide 'init-reveal)
;;; init-reveal.el ends here
