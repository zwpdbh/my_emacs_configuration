(when (maybe-require-package 'slime)
  (maybe-require-package 'helm-slime)
  
  (when (maybe-require-package 'slime-company)
    (defun zw/set-company-backends-with-slime ()
      (interactive)
      (message "set zwpdbh's company-backends")
      (setq-local company-backends '((company-slime company-capf company-bbdb company-dabbrev-code))))

    (after-load 'slime-company
      (setq slime-company-completion 'fuzzy
            slime-company-after-completion 'slime-company-just-one-space)
      (zw/set-company-backends-with-slime))
    (add-hook 'slime-repl-mode-hook 'zw/set-company-backends-with-slime))
  
  (setq slime-contribs '(slime-fancy
                         slime-asdf
                         slime-autodoc
                         slime-editing-commands
                         slime-references
                         slime-repl
                         slime-scratch
                         slime-xref-browser
                         slime-company
                         helm-slime))
  
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


(after-load 'slime
  (setq slime-complete-symbol*-fancy t
        slime-fuzzy-completion-in-place t
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-when-complete-filename-expand t
        slime-truncate-lines nil
        slime-autodoc-use-multiline-p t)
  
  (define-key slime-mode-map  (kbd "C-c C-c") nil)
  (define-key slime-mode-map  (kbd "C-c C-c") #'slime-eval-last-expression)
  (define-key slime-mode-map  (kbd "C-c C-e") nil)
  (define-key slime-mode-map  (kbd "C-c C-e") #'slime-eval-last-expression-in-repl))


(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))


(provide 'init-common-lisp-with-slime)
