;; For ReasonML
(when (maybe-require-package 'reason-mode)
  (add-to-list 'auto-mode-alist '("\\.re$" . reason-mode))
  (add-to-list 'auto-mode-alist '("\\.rei$" . reason-mode))
  
  (defun shell-cmd (cmd)
    "Returns the stdout output of a shell command or nil if the command returned
   an error"
    (car (ignore-errors (apply 'process-lines (split-string cmd)))))

  (defun reason-cmd-where (cmd)
    (let ((where (shell-cmd cmd)))
      (if (not (string-equal "unknown flag ----where" where))
          where)))

  (let* ((refmt-bin (or (reason-cmd-where "refmt ----where")
                        (shell-cmd "which refmt")
                        (shell-cmd "which bsrefmt")))
         (merlin-bin (or (reason-cmd-where "ocamlmerlin ----where")
                         (shell-cmd "which ocamlmerlin")))
         (merlin-base-dir (when merlin-bin
                            (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
    ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when merlin-bin
      (setq merlin-command merlin-bin))

    (when refmt-bin
      (setq refmt-command refmt-bin)))

  (add-hook 'reason-mode-hook
            (lambda ()
              (my/disable-paredit-spaces-before-paren)
              (paredit-mode t)
              (zw/counsel-etags-setup)
              (add-hook 'before-save-hook 'refmt nil 'local)              
              (zw/set-company-backends-for-ocaml))))

;; ref: https://gist.github.com/Khady/d37b7d88c81c4178dcccc6579fd0b526
(when (maybe-require-package 'utop)
  (add-hook 'utop-mode-hook
            (lambda ()
              ;; make sure it doesn't affect global company-backends
              (setq company-backends (zw/delete-from-company-backends 'utop-company-backend))
              (setq-local company-backends (zw/add-to-company-backends 'utop-company-backend))))

  ;; Need opam install utop rtop
  ;; However, currently .ocamlinit's format is not compartible with rtop
  ;; So, utop with rtop could not be started correctly.
  (setq utop-command "opam config exec -- rtop -emacs") 
  (add-hook 'reason-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-c C-c"))
              (local-unset-key (kbd "C-c C-e"))
              (define-key reason-mode-map (kbd "C-c C-c") 'utop-eval-phrase)
              (define-key reason-mode-map (kbd "C-c C-e") 'utop-eval-buffer)))
  (add-hook 'reason-mode-hook #'utop-minor-mode))