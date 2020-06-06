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
  ;; After connection, use (lisp-implementation-type) to check the connected common-lisp implementation.
  ;; quicklisp
  ;; https://www.quicklisp.org/beta/#installation is library manager for Common Lisp
  ;; Loading after installation: (load "~/quicklisp/setup.lisp")
  ;; To load Quicklisp when you start Lisp: (ql:add-to-init-file)
  (cond
   (my-roswell
    ;; Use ros default cl implementation.
    ;; To switch different implementations, use ros use ccl-bin, or ros use sbcl/2.0.2
    (setq inferior-lisp-program (concat my-roswell " -Q run")))
   (my-ccl
    (setq inferior-lisp-program my-ccl)
    (setq sly-lisp-implementations `((ccl (,my-ccl)))))
   (my-sbcl
    (setq inferior-lisp-program my-sbcl)
    (setq sly-lisp-implementations `((sbcl (,my-sbcl)))))
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
;; use Ctrl+up to call previous input
(add-hook 'sly-mrepl-mode-hook
          #'(lambda ()
              (define-key sly-mrepl-mode-map (kbd "C-<up>") 'sly-mrepl-previous-input-or-button)))

;; ;; How to indent keywords aligned?
;; ;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned/52789#52789
;; (advice-add #'calculate-lisp-indent :override #'void~calculate-lisp-indent)
;; ;; ;; Define indent cases for symbols in common-lisp
;; (eval-after-load 'cl-indent
;;   `(progn
;;      (put 'cl-flet 'common-lisp-indent-function
;;       (get 'flet 'common-lisp-indent-function))
;;      (put 'cl-labels 'common-lisp-indent-function
;;       (get 'labels 'common-lisp-indent-function))
;;      (put 'if 'common-lisp-indent-function 2)))


(provide 'init-common-lisp)
