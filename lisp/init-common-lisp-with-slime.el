(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(when (maybe-require-package 'slime)  
  ;; (require 'slime-autoloads)
  ;; set some contribs
  ;; If met error: not found file or directory slime-fancy,
  ;; make sure lisp system works first, then restart Emacs and try again for several times.
  (setq slime-contribs '(slime-fancy
                         slime-cl-indent
                         slime-autodoc
                         slime-editing-commands
                         slime-references
                         slime-repl
                         slime-scratch
                         slime-xref-browser))
  (slime-setup))

(when (maybe-require-package 'helm-slime)
  (add-to-list 'slime-contribs 'helm-slime))

(when (maybe-require-package 'slime-company)
  ;; when press TAB during company completion, slime will enter one space automatically
  (setq slime-company-after-completion nil)
  (setq slime-company-completion 'simple)
  (setq slime-complete-symbol*-fancy t)
  (setq slime-completion-at-point-functions 'slime-simple-completion-at-point)
  (setq slime-when-complete-filename-expand t)
  (setq slime-truncate-lines nil)
  (setq slime-autodoc-use-multiline-p t)

  (require 'slime-company))

;; set company-slime into company-backends properly
(defun zw/set-company-slime ()
  ;; company-capf should put at behind because it cause too many extra candidates
  (setq-local company-backends (zw/delete-from-company-backends 'company-capf))
  (setq-local company-backends (zw/delete-from-company-backends 'company-yasnippet))
  (setq-local company-backends (zw/add-to-company-backends 'company-slime)))

(add-hook 'slime-repl-mode-hook 'zw/set-company-slime)
(add-hook 'slime-mode-hook 'zw/set-company-slime)
(add-hook 'lisp-mode-hook
          '(lambda ()
            (setq lisp-indent-function 'common-lisp-indent-function)))

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


;; Set the alignment of the indents under him
;; Learn more about customization of this case you can read
;; SLIME source, namely
;; %путь_к_slime%/contrib/slime-cl-indent.el
(define-common-lisp-style "zw/common-lisp-indent-style"
  "My custom indent style."
  (:inherit "modern")
  (:variables
   (lisp-loop-indent-subclauses t)) 
  (:indentation
   (if (4 2 2))
   (define (&lambda 2))
   (with-gensyms ((&whole 4 &rest 1) &body))
   (once-only (as with-gensyms))))
(setq common-lisp-style-default "zw/common-lisp-indent-style")

(after-load 'slime
            (defun zw/slime-eval-last-expression-in-repl ()
              "zw customize slime-eval to make sure to jump back to original buffer"
              (interactive) 
              (let ((buf (buffer-name)))
                (save-excursion
                 (slime-eval-last-expression-in-repl t)
                 (select-window (previous-window)))))
            
            (if (and (search "sbcl" inferior-lisp-program)
                     (search "sbcl" (car (car (cdr (car slime-lisp-implementations))))))
                (progn
                  (setq common-lisp-style-default "sbcl")
                  (setq common-lisp-style "sbcl"))
                (progn
                  (setq common-lisp-style-default "modern")
                  (setq common-lisp-style "modern")))

            (local-unset-key (kbd "C-c C-c"))
            (local-unset-key (kbd "C-c C-e"))
            
            (define-key slime-mode-map (kbd "C-c C-c") #'slime-eval-last-expression)
            ;; (define-key slime-mode-map (kbd "C-c C-c") #'zw/slime-eval-last-expression-in-repl)
            
            (define-key slime-mode-map (kbd "C-c C-e") #'slime-eval-last-expression-in-repl)
            
            (define-key slime-mode-map (kbd "C-c C-l") #'slime-load-file)
            (define-key slime-mode-map (kbd "C-c C-h") #'slime-documentation-lookup)
            (define-key slime-mode-map (kbd "C-c t") #'slime-toggle-trace-fdefinition))


(provide 'init-common-lisp-with-slime)
