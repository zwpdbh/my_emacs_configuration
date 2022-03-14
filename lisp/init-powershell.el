(when (maybe-require-package 'powershell)
  (after-load 'org
    ;; create our dummy one 
    (defun org-babel-execute:powershell (body params) body)
    ;; notice: it is case sensitive
    (add-to-list 'org-structure-template-alist '("ps" . "src powershell"))
    (add-to-list 'org-structure-template-alist '("powershell" . "src powershell"))))
(provide 'init-powershell)
