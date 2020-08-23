(setq zw/common-lisp-use-slime 't)
(if zw/common-lisp-use-slime
    (progn
      (require 'init-common-lisp-with-slime)
      (message "use slime for common-lisp"))
  (progn
    (require 'init-common-lisp)
    (message "use sly for common-lisp")))


(provide 'init-common-lisp-configure)