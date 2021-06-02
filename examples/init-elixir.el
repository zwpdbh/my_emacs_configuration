;; ref: https://erickgnavar.github.io/emacs-config/#org4e744a7

(reformatter-define elixir-format
  :program "mix"
  :args '("format" "-")
  :group 'elixir)

(use-package elixir-mode
  :ensure t
  :bind (:map elixir-mode-map
              ("C-c C-t" . 'my/mix-run-test-at-point)
              ("C-c C-f" . elixir-format-buffer))
  :config
  ;; (evil-leader/set-key-for-mode 'elixir-mode "d" 'dumb-jump-go)
  )

;; Custom functions to run elixir tests.
;; elixir-extra-test-env can be set up on .dir-locals.el

(defun my/mix-run-test (&optional scope)
  "Run elixir test for the given SCOPE."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (current-line (line-number-at-pos))
         (possible-mix-paths `(,(concat (my/project-root) "mix.exs")
                               ,(concat (my/project-root) "src/mix.exs")))
         (mix-file (car (seq-filter 'file-exists-p possible-mix-paths)))
         (default-directory (file-name-directory mix-file))
         (extra-env (if (boundp 'elixir-extra-test-env) elixir-extra-test-env ""))
         (mix-env (concat "MIX_ENV=test " extra-env)))

    (cond
     ((string-equal scope "file")
      (compile (format "%s mix test %s" mix-env current-file)))

     ((string-equal scope "at-point")
      (compile (format "%s mix test %s:%s" mix-env current-file current-line)))

     (t
      (compile (format "%s mix test" mix-env))))))


(defun my/mix-run-test-file ()
  "Run mix test over the current file."
  (interactive)
  (my/mix-run-test "file"))

(defun my/mix-run-test-at-point ()
  "Run mix test at point."
  (interactive)
  (my/mix-run-test "at-point"))

(defun my/mix-run-test-all ()
  "Run mix test at point."
  (interactive)
  (my/mix-run-test))

(provide 'init-elixir.el)
;;; init-elixir ends here