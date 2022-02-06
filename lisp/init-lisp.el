(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))

(setq zw/common-lisp-use-slime 't)
(if zw/common-lisp-use-slime
    (progn
      (require 'init-common-lisp-with-slime)
      (message "use slime for common-lisp"))
  (progn
    (require 'init-common-lisp)
    (message "use sly for common-lisp")))


(provide 'init-lisp)