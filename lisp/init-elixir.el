;; ref: https://www.badykov.com/emacs/2020/05/30/emacs-setup-for-elixir/
;; ref: https://erick.navarro.io/blog/minimal-setup-for-elixir-development-in-emacs/

(when (maybe-require-package 'elixir-mode)
  (add-hook 'elixir-mode-hook
            (lambda ()
              (my/disable-paredit-spaces-before-paren)
              (paredit-mode t)
              (add-hook 'before-save-hook 'zw/indent-buffer nil 'local))))

(when (maybe-require-package 'alchemist)  
  ;; ref: https://alchemist.readthedocs.io/en/latest/installation/
  ;; ref: https://develop.spacemacs.org/layers/+lang/elixir/README.html#layer
  (setq alchemist-iex-program-name (executable-find "iex")) 
  (setq alchemist-execute-command (executable-find "elixir")) 
  (setq alchemist-compile-command (executable-find "elixirc"))

  (setq alchemist-mix-command (executable-find "mix"))
  (setq alchemist-mix-test-task (executable-find "espec"))

  (add-hook 'elixir-mode-hook
            (lambda ()
              (alchemist-mode t)
              (define-key (current-local-map) (kbd "C-c C-c") 'alchemist-eval-buffer)
              (define-key (current-local-map) (kbd "C-c C-e") 'alchemist-eval-print-current-line))))


(when (maybe-require-package 'ob-elixir)
  (after-load 'org
    (add-to-list 'zw/org-babel-evaluate-whitelist "elixir")
    (add-to-list 'zw/org-babel-load-language-list '(elixir . t))
    (add-to-list 'org-structure-template-alist '("exs" . "src elixir"))))


(provide 'init-elixir)
;;; init-elixir ends here