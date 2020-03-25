(use-package eldoc
  :diminish
  :defer t
  :ensure t
  :init
  ;; highlight eldoc arguments in emacslisp
  (defun eldoc-get-arg-index ()
    (save-excursion
      (let ((fn (eldoc-fnsym-in-current-sexp))
            (i 0))
        (unless (memq (char-syntax (char-before)) '(32 39)) ; ? , ?'
          (condition-case err
              (backward-sexp)             ;for safety
            (error 1)))
        (condition-case err
            (while (not (equal fn (eldoc-current-symbol)))
              (setq i (1+ i))
              (backward-sexp))
          (error 1))
        (max 0 i))))

  (defun eldoc-highlight-nth-arg (doc n)
    (cond ((null doc) "")
          ((<= n 0) doc)
          (t
           (let ((i 0))
             (mapconcat
              (lambda (arg)
                (if (member arg '("&optional" "&rest"))
                    arg
                  (prog2
                      (if (= i n)
                          (put-text-property 0 (length arg) 'face 'underline arg))
                      arg
                    (setq i (1+ i)))))
              (split-string doc) " ")))))

  (defadvice eldoc-get-fnsym-args-string (around highlight activate)
    ""
    (setq ad-return-value (eldoc-highlight-nth-arg ad-do-it
                                                   (eldoc-get-arg-index))))
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(provide 'init-eldoc)
