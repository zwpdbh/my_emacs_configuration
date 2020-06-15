(when (maybe-require-package 'slime)
  (when (maybe-require-package 'slime-company)
    (slime-setup '(slime-fancy slime-company)))
  
  (if *win64*
      (setq my-ccl (executable-find "wx86cl64"))
    (setq my-ccl (executable-find "ccl")))
  (setq my-sbcl (executable-find "sbcl"))
  (setq my-clisp (executable-find "clisp"))
  (setq my-roswell (executable-find "ros"))

  (cond
   (my-roswell
    ;; Use ros default cl implementation.
    ;; To switch different implementations, use ros use ccl-bin, or ros use sbcl/2.0.2
    (setq inferior-lisp-program (concat my-roswell " -Q run")))
   (my-ccl
    (setq inferior-lisp-program my-ccl)
    (setq slime-lisp-implementations `((ccl (,my-ccl)))))
   (my-sbcl
    (setq inferior-lisp-program my-sbcl)
    (setq slime-lisp-implementations `((sbcl (,my-sbcl)))))
   (my-clisp
    (setq inferior-lisp-program my-clisp)
    (setq slime-lisp-implementations `((clisp (,my-clisp)))))))

(after-load 'slime-company
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))


(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))


(provide 'init-common-lisp-with-slime)