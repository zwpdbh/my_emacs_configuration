
(when (maybe-require-package 'plantuml-mode)
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (setq plantuml-output-type "svg")
  ;; needed by ob-plantuml.el
  (setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(add-hook 'plantuml-mode-hook '(lambda ()
                                 (setq-local company-backends (add-to-list 'company-backends 'plantuml-complete-symbol))))

(after-load 'org
  (add-to-list 'org-structure-template-alist '("plantuml" . "src plantuml"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t))))

(provide 'init-plantuml)