;; ref: https://gist.github.com/dbuenzli/a797e398cb3f6503b6e0b5f34249648a

;; sudo apt install opam (OCaml package manager)
;; opam install caml-mode merlin ocp-indent
(add-to-list 'load-path "~/.opam/system/share/emacs/site-lisp/")

(when (maybe-require-package 'tuareg)
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'ocamldebug "ocamldebug" "Run the Caml debugger" t)
  (autoload 'caml-mode "caml" "Major mode for editing OCaml code." t)
  (autoload 'run-caml "inf-caml" "Run an inferior OCaml process." t)
  
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . tuareg-mode))
  (add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))
  (add-to-list 'interpreter-mode-alist '("ocamlrun" . caml-mode))
  (add-to-list 'interpreter-mode-alist '("ocaml" . caml-mode))

  (add-hook 'tuareg-mode-hook
            (lambda ()
                (define-key tuareg-mode-map (kbd "C-c C-c") 'tuareg-eval-phrase)
                (define-key tuareg-mode-map (kbd "C-c C-e") 'tuareg-eval-region)))
  
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
  (require 'caml-types nil 'noerror)
  
  (setq merlin-use-auto-complete-mode 'easy)
  (setq merlin-command 'opam)
  (setq merlin-error-on-single-line t)
  
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'caml-mode-hook #'merlin-mode))


(when (maybe-require-package 'merlin-company)
  (add-hook 'caml-mode-hook
            (lambda ()
              (setq-local company-backends (zw/add-to-company-backends company-backends 'merlin-company-backend)))))

(provide 'init-ocaml)
