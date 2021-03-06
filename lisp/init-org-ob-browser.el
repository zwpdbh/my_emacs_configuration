
;; Must install PhantomJS - Scriptable Headless Browser on system
(when (maybe-require-package 'ob-browser)
  (after-load 'org
    (add-to-list 'zw/org-babel-evaluate-whitelist "browser")
    (add-to-list 'zw/org-babel-load-language-list '(browser . t))
    (add-to-list 'org-structure-template-alist '("browser" . "src browser :out demo.png"))

    ;; since css mode is not supported by org source block, create the command yourself
    (defun org-babel-execute:css (body params) body)
    
    (add-to-list 'zw/org-babel-load-language-list '(css . t))
    (add-to-list 'org-structure-template-alist '("css" . "src css"))))

(provide 'init-org-ob-browser)