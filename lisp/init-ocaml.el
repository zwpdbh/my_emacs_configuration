;; ref: https://gist.github.com/dbuenzli/a797e398cb3f6503b6e0b5f34249648a
;; ref: https://ocaml.xyz/book/install.html
;; MUST INSTALL opam
;; sudo apt install opam (OCaml package manager)
;; opam install caml-mode merlin ocamlformat

;; Prepare opam related path
;; opam-share contains files related to configuration, such as emacs plugins
(setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
;; opam-bin contains executable files installed by opam
(setq opam-bin (substring (shell-command-to-string "opam config var bin") 0 -1))

(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(add-to-list 'exec-path opam-bin)
;; (load-file (concat (concat opam-share "/emacs/site-lisp") "/ocp-indent.el"))



(defun zw/set-company-backends-for-ocaml ()
  (interactive)
  (merlin-mode t)
  (setq-local company-backends (zw/delete-from-company-backends 'company-capf))
  (setq-local company-backends (zw/delete-from-company-backends 'merlin-company-backend))
  ;; remember to comment out the autmatically register company-backends in merlin-company.el
  (setq-local company-backends (zw/add-to-company-backends 'merlin-company-backend)))


;; ref: https://github.com/ocaml/merlin/wiki/emacs-from-scratch
;; opam install merlin
(when (maybe-require-package 'merlin)
  (maybe-require-package 'merlin-company)
  
  (setq merlin-command (concat opam-bin "/ocamlmerlin"))
  (autoload 'merlin-mode "merlin" "Merlin mode" t)
  
  (require 'caml-types nil 'noerror)
  (setq merlin-error-on-single-line t)

  (after-load 'merlin
    (define-key merlin-mode-map
      (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
    (define-key merlin-mode-map
      (kbd "C-c <down>") 'merlin-type-enclosing-go-down)))


(defun zw/set-paredit-for-ocaml ()
  (my/disable-paredit-spaces-before-paren)
  (paredit-mode t)
  (define-key tuareg-mode-map (kbd "}") 'paredit-close-curly))

(when (executable-find "ocaml")
  (when (maybe-require-package 'tuareg)
    (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
    (autoload 'ocamldebug "ocamldebug" "Run the Caml debugger" t)
    (autoload 'caml-mode "caml" "Major mode for editing OCaml code." t)
    (autoload 'run-caml "inf-caml" "Run an inferior OCaml process." t)

    (setq tuareg-interactive-program "ocaml -nopromptcont")
    (setq tuareg-indent-align-with-first-arg t)
    
    (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . tuareg-mode))
    (add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))
    (add-to-list 'interpreter-mode-alist '("ocamlrun" . caml-mode))
    (add-to-list 'interpreter-mode-alist '("ocaml" . caml-mode))

    (if window-system
        (progn
          (require 'caml-font)
          (set-face-foreground 'caml-font-doccomment-face "#cb4b16"))))

  (add-hook 'tuareg-interactive-mode-hook
            (lambda ()
              (zw/set-company-backends-for-ocaml)
              (zw/set-paredit-for-ocaml)))

  ;; opam install ocp-indent
  (require 'ocp-indent)
  (add-hook 'tuareg-mode-hook
            (lambda ()
              (zw/set-company-backends-for-ocaml)
              ;; (zw/set-paredit-for-ocaml)
              (local-unset-key (kbd "C-c C-c"))
              (local-unset-key (kbd "C-c C-e"))
              (define-key tuareg-mode-map (kbd "C-c C-c") 'tuareg-eval-phrase)
              (define-key tuareg-mode-map (kbd "C-c C-e") 'tuareg-eval-buffer)

              (zw/counsel-etags-setup)
              (add-hook 'before-save-hook #'ocp-indent-buffer nil 'local)))
  
  (add-hook 'ocaml-mode-hook
            (lambda ()
              (merlin-mode t)
              (zw/set-company-backends-for-ocaml)))

  (after-load 'org
    (add-to-list 'zw/org-babel-evaluate-whitelist "ocaml")
    (add-to-list 'zw/org-babel-load-language-list '(ocaml . t))
    (add-to-list 'org-structure-template-alist '("ml" . "src ocaml :results verbatim"))))



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
  (after-load 'utop
              (setq company-backends (zw/delete-from-company-backends 'utop-company-backend)))
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

(provide 'init-ocaml)
