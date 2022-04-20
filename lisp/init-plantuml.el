(when (maybe-require-package 'plantuml-mode)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (setq plantuml-output-type "png"))

(add-hook 'plantuml-mode-hook #'(lambda ()
                                 (setq-local company-backends (add-to-list 'company-backends 'plantuml-complete-symbol))))

(after-load 'org
  ;; needed by ob-plantuml.el
  (setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (add-to-list 'org-structure-template-alist '("plantuml" . "src plantuml")))

(provide 'init-plantuml)
