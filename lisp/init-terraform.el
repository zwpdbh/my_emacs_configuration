(use-package terraform-mode
  :commands (terraform-mode)
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(use-package company-terraform
  :ensure t
  :after (terraform-mode))

(defun zw/terraform-mode-hook ()
  ;; (setq-local company-backends (append '(company-terraform) company-backends))
  (set (make-local-variable 'company-backends) 
       '((company-bbdb company-terraform company-capf company-dabbrev-code)))
  (setq terraform-indent-level 2))

(add-hook 'terraform-mode-hook '(lambda ()
                                  (zw/terraform-mode-hook)))

(provide 'init-terraform)
