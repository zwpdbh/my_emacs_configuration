(use-package sly
  ;; use ~sly~ to connect to REPL
  :ensure t
  :defer t
  :init
  (setq sly-net-coding-system 'utf-8-unix)

  (if *win64*
      (setq my-ccl (executable-find "wx86cl64"))
    (setq my-ccl (executable-find "ccl")))
  (setq my-sbcl (executable-find "sbcl"))
  (setq my-clisp (executable-find "clisp"))
  (setq my-roswell (executable-find "ros"))
  
  (cond
   (my-roswell
    (setq inferior-lisp-program "ros -Q run"))
   (my-sbcl
    (setq inferior-lisp-program my-sbcl)
    (setq sly-lisp-implementations `((sbcl (,my-sbcl)))))
   (my-ccl
    (setq inferior-lisp-program my-ccl)
    (setq sly-lisp-implementations `((ccl (,my-ccl)))))
   (my-clisp
    (setq inferior-lisp-program my-clisp)
    (setq sly-lisp-implementations `((clisp (,my-clisp))))))
  :commands (sly-mode))

(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(add-hook 'sly-mode-hook 
          #'(lambda ()
              (define-key sly-editing-mode-map (kbd "C-c C-c") nil)
              (define-key sly-editing-mode-map (kbd "C-c C-c") 'sly-eval-last-expression)))

;; After connection, use (lisp-implementation-type) to check the connected common-lisp implementation.
;; quicklisp
;; https://www.quicklisp.org/beta/#installation is library manager for Common Lisp
;; Loading after installation: (load "~/quicklisp/setup.lisp")
;; To load Quicklisp when you start Lisp: (ql:add-to-init-file)

(provide 'init-common-lisp)
