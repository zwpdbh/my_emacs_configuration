;; ref: https://erickgnavar.github.io/emacs-config/#org4e744a7
;; ref: https://www.badykov.com/emacs/2020/05/30/emacs-setup-for-elixir/
;; ref: https://erick.navarro.io/blog/minimal-setup-for-elixir-development-in-emacs/

(when (and
       (executable-find "iex")
       (executable-find "elixir")
       (executable-find "elixirc"))
  
  (when (maybe-require-package 'elixir-mode)
    (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
    (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
    
    (add-hook 'elixir-mode-hook
              (lambda ()
                ;; === Notice, on windows we may need to install: choco install diffutils, if we meet error: missing diff
                (add-hook 'before-save-hook 'elixir-format nil 'local)
                (my/disable-paredit-spaces-before-paren)
                (when (fboundp 'zw/consult-ripgrep-at-point)
                  (define-key elixir-mode-map (kbd "M-/") 'zw/consult-ripgrep-at-point)))))

  (defun zw/set-company-backends-for-elixir ()
    (interactive)
    ;; First remove it since alchemist-mode automatically add its backend direct to company-backends
    (setq-local company-backends (zw/delete-from-company-backends 'alchemist-company))
    ;; Then, add its backend properly into my global backends
    (setq-local company-backends (zw/add-to-company-backends 'alchemist-company))
    (setq-local company-backends (zw/delete-from-company-backends 'company-capf)))

  (when (maybe-require-package 'alchemist)  
    ;; ref: https://alchemist.readthedocs.io/en/latest/installation/
    ;; ref: https://alchemist.readthedocs.io/en/latest/basic_usage/
    ;; ref: https://develop.spacemacs.org/layers/+lang/elixir/README.html#layer
    ;; ref: https://medium.com/@vasspilka/basic-workflow-for-alchemist-el-c35f460e04e1
    
    (setq alchemist-iex-program-name (executable-find "iex")) 
    (setq alchemist-execute-command (executable-find "elixir")) 
    (setq alchemist-compile-command (executable-find "elixirc"))

    (setq alchemist-mix-command (executable-find "mix"))
    (setq alchemist-mix-test-task (executable-find "espec"))

    (defun zw/alchemist-iex-send-last-sexp ()
      (interactive)
      (save-excursion
        (call-interactively 'alchemist-iex-send-last-sexp)
        (display-buffer (process-buffer (alchemist-iex-process)))))

    (defun zw/alchemist-iex-send-region (beg end)
      "Sends the marked region to the IEx process."
      (interactive (list (point) (mark)))
      (unless (and beg end)
        (error "The mark is not set now, so there is no region"))
      (save-excursion
        (let* ((region (buffer-substring-no-properties beg end)))
          (alchemist-iex--send-command (alchemist-iex-process) region)
          (display-buffer (process-buffer (alchemist-iex-process)))
          (deactivate-mark))))

    (defun zw/alchemist-iex-compile-this-buffer ()
      (interactive)
      (save-excursion
        (alchemist-iex-compile-this-buffer)
        (display-buffer (process-buffer (alchemist-iex-process)))))

    (add-hook 'alchemist-iex-mode-hook
              (lambda ()
                (zw/set-company-backends-for-elixir)))

    (defun zw/insert-elixir-pipe-operator ()
      "Insert a newline and the |> operator"
      (interactive)
      (end-of-line)
      (newline-and-indent)
      (insert "|> "))

    
    (add-hook 'elixir-mode-hook
              (lambda ()
                (alchemist-mode t)
                (zw/set-company-backends-for-elixir)
                
                (define-key elixir-mode-map (kbd "<M-return>") 'zw/insert-elixir-pipe-operator)
                (define-key (current-local-map) (kbd "C-c C-c") 'zw/alchemist-iex-send-last-sexp)
                (define-key (current-local-map) (kbd "C-c C-e") 'zw/alchemist-iex-send-region)
                (define-key (current-local-map) (kbd "C-c C-l") 'zw/alchemist-iex-compile-this-buffer)
                (define-key elixir-mode-map (kbd "C-c e") 'zw/alchemist-iex-send-region)

                (define-key elixir-mode-map (kbd "C-c m") 'alchemist-macroexpand-current-line)
                (define-key elixir-mode-map (kbd "C-c r") 'alchemist-macroexpand-region)
                (define-key elixir-mode-map (kbd "C-c C-p") 'alchemist-macroexpand-print-current-line)
                (define-key elixir-mode-map (kbd "C-c C-r") 'alchemist-macroexpand-print-region)

                (define-key elixir-mode-map (kbd "C-c a i p") 'alchemist-iex-project-run)
                
                (define-key (current-local-map) (kbd "<f1>") 'zw/alchemist-iex-send-region)
                (define-key (current-local-map) (kbd "<f10>") 'elixir-format))))


  (defun zw/mix-run-test (&optional at-point)
    "If AT-POINT is true it will pass the line number to mix test."
    (interactive)
    (let* ((current-file (buffer-file-name))
	         (current-line (line-number-at-pos))
	         (mix-file (concat (projectile-project-root) "mix.exs"))
	         (default-directory (file-name-directory mix-file))
	         (mix-env (concat "MIX_ENV=test ")))

      (if at-point
	        (compile (format "%s mix test %s:%s" mix-env current-file current-line))
        (compile (format "%s mix test %s" mix-env current-file)))))

  (defun zw/mix-run-test-file ()
    "Run mix test over the current file."
    (interactive)
    (zw/mix-run-test nil))

  (defun zw/mix-run-test-at-point ()
    "Run mix test at point."
    (interactive)
    (zw/mix-run-test t))


  (when (maybe-require-package 'ob-elixir)
    (after-load 'org
      (add-to-list 'zw/org-babel-evaluate-whitelist "elixir")
      (add-to-list 'zw/org-babel-load-language-list '(elixir . t))
      (add-to-list 'org-structure-template-alist '("exr" . "src elixir"))
      (add-to-list 'org-structure-template-alist '("exs" . "src elixir")))))


(provide 'init-elixir)
;;; init-elixir ends here
