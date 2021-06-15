;; ref: https://www.badykov.com/emacs/2020/05/30/emacs-setup-for-elixir/
;; ref: https://erick.navarro.io/blog/minimal-setup-for-elixir-development-in-emacs/

(when (maybe-require-package 'elixir-mode)
  (add-hook 'elixir-mode-hook
            (lambda ()
              (my/disable-paredit-spaces-before-paren)
              (paredit-mode t)
              (add-hook 'before-save-hook 'indent-according-to-mode nil 'local))))

(defun zw/set-company-backends-for-elixir ()
  (interactive)
  (setq-local company-backends (zw/delete-from-company-backends 'company-capf))
  ;; First remove it since alchemist-mode automatically add its backend direct to company-backends
  (setq-local company-backends (zw/delete-from-company-backends 'alchemist-company))
  ;; Then, add its backend properly into my global backends
  (setq-local company-backends (zw/add-to-company-backends 'alchemist-company)))

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
        (display-buffer (process-buffer (alchemist-iex-process)))
        (deactivate-mark))))

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
  
  (add-hook 'elixir-mode-hook
            (lambda ()
              (alchemist-mode t)
              (zw/set-company-backends-for-elixir)
              (define-key (current-local-map) (kbd "C-c C-c") 'zw/alchemist-iex-send-current-line)
              (define-key (current-local-map) (kbd "C-c C-e") 'zw/alchemist-iex-compile-this-buffer)
              (define-key (current-local-map) (kbd "C-c C-l") 'zw/alchemist-iex-compile-this-buffer)
              (define-key (current-local-map) (kbd "C-c C-r") 'zw/alchemist-iex-send-region)
              (define-key (current-local-map) (kbd "<f1>") 'zw/alchemist-iex-send-region)
              (define-key (current-local-map) (kbd "<f10>") 'elixir-format))))


(when (maybe-require-package 'ob-elixir)
  (after-load 'org
    (add-to-list 'zw/org-babel-evaluate-whitelist "elixir")
    (add-to-list 'zw/org-babel-load-language-list '(elixir . t))
    (add-to-list 'org-structure-template-alist '("exs" . "src elixir"))))


(provide 'init-elixir)
;;; init-elixir ends here