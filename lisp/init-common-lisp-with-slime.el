(when (maybe-require-package 'slime)  
  (require 'slime-autoloads)
  ;; set some contribs
  ;; If met error: not found file or directory slime-fancy,
  ;; make sure lisp system works first, then restart Emacs and try again for several times.
  (setq slime-contribs '(slime-fancy
                         slime-autodoc
                         slime-editing-commands
                         slime-references
                         slime-repl
                         slime-scratch
                         slime-xref-browser)))

(when (maybe-require-package 'helm-slime)
  (add-to-list 'slime-contribs 'helm-slime))

(when (maybe-require-package 'slime-company)
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space)
  
  ;; set company-slime into company-backends properly
  (defun zw/set-company-slime ()
    (interactive)
    ;; company-capf should put at behind because it cause too many extra candidates
    (setq-local company-backends '((company-dabbrev-code company-slime) company-capf company-keywords company-files company-dabbrev))
    
    (setq slime-complete-symbol*-fancy t
          ;; options are slime-simple-completion-at-point, slime-complete-symbol*, slime-fuzzy-complete-symbol
          slime-completion-at-point-functions 'slime-simple-completion-at-point
          slime-when-complete-filename-expand t
          slime-truncate-lines nil
          slime-autodoc-use-multiline-p t)
    (add-to-list 'slime-contribs 'slime-company)))

(after-load 'slime-company
  (zw/set-company-slime))

(after-load 'slime
  (require 'slime-cl-indent)
  (add-to-list 'slime-contribs 'slime-cl-indent)
  (setq common-lisp-style-default "classic")
  
  (define-key slime-mode-map  (kbd "C-c C-c") nil)
  (define-key slime-mode-map  (kbd "C-c C-c") #'slime-eval-last-expression)
  (define-key slime-mode-map  (kbd "C-c C-e") nil)
  (define-key slime-mode-map  (kbd "C-c C-e") #'slime-eval-last-expression-in-repl)
  (define-key slime-mode-map (kbd "C-c C-h") #'slime-documentation-lookup))

;; (add-hook 'lisp-mode-hook 'zw/set-company-slime)
(add-hook 'slime-repl-mode-hook 'zw/set-company-slime)
(add-hook 'slime-mode-hook 'zw/set-company-slime)

(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

;; set lisp system
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
  (setq slime-lisp-implementations `((clisp (,my-clisp))))))


(provide 'init-common-lisp-with-slime)
