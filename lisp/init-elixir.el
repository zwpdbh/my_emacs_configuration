;; ref: https://www.badykov.com/emacs/2020/05/30/emacs-setup-for-elixir/
;; ref: https://erick.navarro.io/blog/minimal-setup-for-elixir-development-in-emacs/

(when (maybe-require-package 'elixir-mode)
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))
  
  (add-hook 'elixir-mode-hook
            (lambda ()
              (my/disable-paredit-spaces-before-paren)
              (paredit-mode t)
              (add-hook 'before-save-hook 'elixir-format nil 'local)))

  (when (maybe-require-package 'inf-elixir)
    (add-hook 'elixir-mode-hook
              (lambda ()
                (inf-elixir-minor-mode t)
                (define-key inf-elixir-mode-map (kbd "C-c C-c") 'inf-elixir-send-line)
                (define-key inf-elixir-mode-map (kbd "C-c C-e") 'inf-elixir-send-buffer)
                (define-key inf-elixir-mode-map (kbd "C-c i p") 'inf-elixir-project)
                (define-key inf-elixir-mode-map (kbd "C-c i i") 'inf-elixir)))))

(when (maybe-require-package 'ob-elixir)
  (after-load 'org
    (add-to-list 'zw/org-babel-evaluate-whitelist "elixir")
    (add-to-list 'zw/org-babel-load-language-list '(elixir . t))
    (add-to-list 'org-structure-template-alist '("exs" . "src elixir"))))



;; see: https://erick.navarro.io/blog/minimal-setup-for-elixir-development-in-emacs/
(defun my/mix-run-test (&optional at-point)
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

(defun my/mix-run-test-file ()
  "Run mix test over the current file."
  (interactive)
  (my/mix-run-test nil))

(defun my/mix-run-test-at-point ()
  "Run mix test at point."
  (interactive)
  (my/mix-run-test t))


(provide 'init-elixir)
;;; init-elixir ends here