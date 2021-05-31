;; ref: https://gist.github.com/dbuenzli/a797e398cb3f6503b6e0b5f34249648a

;; sudo apt install opam (OCaml package manager)
;; opam install caml-mode merlin ocp-indent
(add-to-list 'load-path "~/.opam/system/share/emacs/site-lisp/")
(add-to-list 'exec-path "~/.opam/system/bin")

(when (executable-find "ocaml")
  (when (maybe-require-package 'tuareg)
    (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
    (autoload 'ocamldebug "ocamldebug" "Run the Caml debugger" t)
    (autoload 'caml-mode "caml" "Major mode for editing OCaml code." t)
    (autoload 'run-caml "inf-caml" "Run an inferior OCaml process." t)
    
    (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . tuareg-mode))
    (add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))
    (add-to-list 'interpreter-mode-alist '("ocamlrun" . caml-mode))
    (add-to-list 'interpreter-mode-alist '("ocaml" . caml-mode))

    (add-hook 'tuareg-interactive-mode-hook
              (lambda ()
                (setq-local company-backends (zw/add-to-company-backends 'merlin-company-backend))
                (setq-local company-backends (zw/delete-from-company-backends 'company-capf))
                (paredit-mode t)))
    
    (after-load 'tuareg
      (set-face-attribute 'tuareg-font-double-semicolon-face nil
                          :foreground "#ffb86c"))
    
    (if window-system
        (progn
          (require 'caml-font)
          (set-face-foreground 'caml-font-doccomment-face "#cb4b16"))))

  (when (maybe-require-package 'ocp-indent)  
    (setq ocp-indent-path
          (concat
           (replace-regexp-in-string "\n$" ""
                                     (shell-command-to-string "opam config var bin")) "/ocp-indent"))
    (setq ocp-indent-config "strict_with=always,match_clause=4,strict_else=never"))

  (when (maybe-require-package 'merlin)
    (autoload 'merlin-mode "merlin" "Merlin mode" t)
    (add-hook 'tuareg-mode-hook #'merlin-mode)
    (add-hook 'caml-mode-hook #'merlin-mode)
    
    (require 'caml-types nil 'noerror)
    (require 'merlin-company)
    
    (setq merlin-use-auto-complete-mode 'easy)
    (setq merlin-command 'opam)
    (setq merlin-error-on-single-line t))

  (add-hook 'tuareg-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-c C-c"))
              (local-unset-key (kbd "C-c C-e"))
              (define-key tuareg-mode-map (kbd "C-c C-c") 'tuareg-eval-phrase)
              (define-key tuareg-mode-map (kbd "C-c C-e") 'tuareg-eval-buffer)

              (paredit-mode t)
              ;; remember to comment out merlin-company auto-appending from merlin-company.el which is shipped with merlin
              (setq-local company-backends (zw/add-to-company-backends 'merlin-company-backend))
              (setq-local company-backends (zw/delete-from-company-backends 'company-capf)))))

(after-load 'org
  (add-to-list 'zw/org-babel-evaluate-whitelist "ocaml")
  (add-to-list 'zw/org-babel-load-language-list '(ocaml . t))
  (add-to-list 'org-structure-template-alist '("ml" . "src ocaml :results verbatim")))


;; For ReasonML
(when (maybe-require-package 'reason-mode)
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
              ;; (add-hook 'before-save-hook 'refmt-before-save nil 'local) ; not very useful
              (add-hook 'before-save-hook 'zw/indent-buffer nil 'local)
              
              ;; remember to comment out merlin-company auto-appending from merlin-company.el which is shipped with merlin
              (setq-local company-backends (zw/add-to-company-backends 'merlin-company-backend))
              (setq-local company-backends (zw/delete-from-company-backends 'company-capf)))))

(provide 'init-ocaml)
