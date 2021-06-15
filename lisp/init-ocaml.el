;; ref: https://gist.github.com/dbuenzli/a797e398cb3f6503b6e0b5f34249648a
;; MUST INSTALL opam
;; sudo apt install opam (OCaml package manager)
;; opam install caml-mode merlin ocamlformat

;; add load-path for ocaml related from opam 
(setq opam-share (shell-command-to-string "opam config var share"))
(setq opam-bin (shell-command-to-string "opam config var bin"))

(when (string-match-p "\n\\'" opam-share)
  (setq opam-share (substring opam-share 0 (- (length opam-share) 1))))
(when (string-match-p "\n\\'" opam-bin)
  (setq opam-bin (substring opam-bin 0 (- (length opam-bin) 1))))

(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; add exec-path for ocaml related from opam
(setq opam-bin (shell-command-to-string "opam config var bin"))
(when (string-match-p "\n\\'" opam-bin)
  (setq opam-bin (substring opam-bin 0 (- (length opam-bin) 1))))
(add-to-list 'exec-path opam-bin)

(defun zw/set-company-backends-for-ocaml ()
  (interactive)
  (setq-local company-backends (zw/delete-from-company-backends 'company-capf))
  (setq-local company-backends (zw/delete-from-company-backends 'merlin-company-backend))
  ;; remember to comment out the autmatically register company-backends in merlin-company.el
  (setq-local company-backends (zw/add-to-company-backends 'merlin-company-backend)))

(when (maybe-require-package 'merlin)
  (autoload 'merlin-mode "merlin" "Merlin mode" t)
  (setq merlin-command (concat opam-bin "/ocamlmerlin"))
  
  (require 'caml-types nil 'noerror)
  (setq merlin-use-auto-complete-mode 'easy)
  (setq merlin-error-on-single-line t))

;; Not very useful
;; (when (maybe-require-package 'utop)
;;   (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
;;   ;; (setq utop-command "opam config exec -- utop -emacs")
;;   (setq utop-command "opam config exec -- dune utop . -- -emacs")
;;   (add-hook 'utop-mode-hook
;;             (lambda ()
;;               ;; Use setq instead of setq-local because utop will change global company-backends.
;;               (setq company-backends (zw/delete-from-company-backends 'utop-company-backend))
;;               (zw/set-company-backends-for-ocaml)))
;;   (add-hook 'tuareg-mode-hook
;;             (lambda ()
;;               (utop-minor-mode t))))

(when (executable-find "ocaml")
  ;; installed by opam install ocamlformat
  ;; (require 'ocamlformat) ; not very useful
  
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

    ;; Disable this customization because the name for this face keep changing for different version causing package load failure.
    ;; (after-load 'tuareg
    ;;   (set-face-attribute 'tuareg-font-double-colon-face nil
    ;;                       :foreground "#ffb86c"))
    
    (if window-system
        (progn
          (require 'caml-font)
          (set-face-foreground 'caml-font-doccomment-face "#cb4b16"))))

  (add-hook 'tuareg-interactive-mode-hook
            (lambda ()
              (zw/set-company-backends-for-ocaml)
              (my/disable-paredit-spaces-before-paren)
              (paredit-mode t)))
  
  (add-hook 'tuareg-mode-hook
            (lambda ()
              (local-unset-key (kbd "C-c C-c"))
              (local-unset-key (kbd "C-c C-e"))
              (define-key tuareg-mode-map (kbd "C-c C-c") 'tuareg-eval-phrase)
              (define-key tuareg-mode-map (kbd "C-c C-e") 'tuareg-eval-buffer)

              (my/disable-paredit-spaces-before-paren)
              (paredit-mode t)
              (define-key tuareg-mode-map (kbd "}") 'paredit-close-curly)
              
              (zw/counsel-etags-setup)
              (add-hook 'before-save-hook #'zw/indent-buffer nil 'local)

              (zw/set-company-backends-for-ocaml)))
  (add-hook 'ocaml-mode-hook #'zw/set-company-backends-for-ocaml))

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
              (my/disable-paredit-spaces-before-paren)
              (paredit-mode t)
              (zw/counsel-etags-setup)
              (add-hook 'before-save-hook 'refmt nil 'local)              
              (zw/set-company-backends-for-ocaml))))

(provide 'init-ocaml)
