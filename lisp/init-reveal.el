;;; init-reveal --- see: https://github.com/yjwen/org-reveal

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (setq org-reveal-mathjax t))


(provide 'init-reveal)
;;; init-reveal.el ends here
