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
  ;; ref: https://alchemist.readthedocs.io/en/latest/basic_usage/
  ;; ref: https://develop.spacemacs.org/layers/+lang/elixir/README.html#layer
  ;; ref: https://medium.com/@vasspilka/basic-workflow-for-alchemist-el-c35f460e04e1
  
  (setq alchemist-iex-program-name (executable-find "iex")) 
  (setq alchemist-execute-command (executable-find "elixir")) 
  (setq alchemist-compile-command (executable-find "elixirc"))

  (setq alchemist-mix-command (executable-find "mix"))
  (setq alchemist-mix-test-task (executable-find "espec"))

  (defun zw/alchemist-iex-send-current-line ()
    (interactive)
    (save-excursion
      (let ((p1 (line-end-position))
            (p2 (line-beginning-position)))
        (set-mark  p1)
        (goto-char p2)
        (call-interactively 'alchemist-iex-send-region)
        (deactivate-mark))))

  (defun zw/alchemist-iex-send-region (beg end)
    "Sends the marked region to the IEx process."
    (interactive (list (point) (mark)))
    (unless (and beg end)
      (error "The mark is not set now, so there is no region"))
    (let* ((region (buffer-substring-no-properties beg end)))
      (alchemist-iex--send-command (alchemist-iex-process) region)
      (deactivate-mark)))
  
  (add-hook 'elixir-mode-hook
            (lambda ()
              (alchemist-mode t)
              (define-key (current-local-map) (kbd "C-c C-e") 'alchemist-iex-compile-this-buffer)
              (define-key (current-local-map) (kbd "C-c C-c") 'zw/alchemist-iex-send-current-line)
              (define-key (current-local-map) (kbd "C-c C-r") 'zw/alchemist-iex-send-region))))


(when (maybe-require-package 'ob-elixir)
  (after-load 'org
    (add-to-list 'zw/org-babel-evaluate-whitelist "elixir")
    (add-to-list 'zw/org-babel-load-language-list '(elixir . t))
    (add-to-list 'org-structure-template-alist '("exs" . "src elixir"))))


(provide 'init-elixir)
;;; init-elixir ends here