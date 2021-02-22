;; (use-package plantuml-mode
;;   :commands (plantuml-mode)
;;   :ensure t
;;   :config
;;   (progn
;;     (setq plantuml-default-exec-mode 'jar)
;;     (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
;;     (setq plantuml-output-type "svg")
;;     ;; needed by ob-plantuml.el
;;     (setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
;;     (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;;     (add-hook 'plantuml-mode-hook '(lambda ()
;;                                      ;; (smartparens-mode)
;;                                      (setq-local company-backends (add-to-list 'company-backends 'plantuml-complete-symbol))))))

(when (maybe-require-package 'plantuml-mode)
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (setq plantuml-output-type "svg")
  ;; needed by ob-plantuml.el
  (setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(add-hook 'plantuml-mode-hook '(lambda ()
                                (setq-local company-backends (add-to-list 'company-backends 'plantuml-complete-symbol))))

(add-hook 'org-mode-hook
          '(lambda ()
            (add-to-list 'zw/org-babel-evaluate-whitelist "plantuml")
            (add-to-list 'zw/org-babel-load-language-list '(plantuml . t))))

(provide 'init-plantuml)