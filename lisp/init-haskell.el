;; references
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md#using-emacs-for-haskell-development

(when (maybe-require-package 'haskell-mode)
  (after-load 'org
    (add-to-list 'zw/org-babel-evaluate-whitelist "haskell")
    (add-to-list 'org-structure-template-alist '("haskell" . "src haskell :results value")))
  
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t)

  ;; ;; If you are using Stack as your build tool, you can also choose to use it for the REPL
  ;; (custom-set-variables '(haskell-process-type 'stack-ghci))  
  (custom-set-variables '(haskell-process-type 'cabal-repl))
  
  ;; Or use current-local-map: (define-key (current-local-map) (kbd "M-.") '<functional-to-call>)
  (after-load 'haskell-mode
    ;; .hs is the common extension for haskell file
    ;; use this command to start haskell-model REPL    
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
    (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

  (after-load 'haskell-cabal
    (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

  (defun zw/haskell-lint ()
    "Run  hlint over the current project."
    (interactive)
    (let ((default-directory (my/project-root)))
      (compile "hlint .")))

  (defun zw/haskell-hlint-buffer ()
    "Run  hlint over the current buffer."
    (interactive)
    (let* ((current-file (buffer-file-name))
           (default-directory (my/project-root)))
      (compile (concat "hlint " current-file))))


  (reformatter-define haskell-format
    :program "hindent"
    :group 'haskell)

  (add-hook 'haskell-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook 'haskell-format-buffer nil 'local))))




;; ;; TODO: install ghc from cabal
;; (when (maybe-require-package 'company-ghc)
;;   (add-to-list 'load-path "~/.cabal/share/ghc-mod-x.y.z")
;;   (autoload 'ghc-init "ghc" nil t)
;;   (autoload 'ghc-debug "ghc" nil t)

;;   (add-hook 'haskell-mode-hook
;;             '(lambda ()
;;                (ghc-init)
;;                (setq-local company-backends '((company-capf company-dabbrev-code company-ghc) company-keywords company-files company-dabbrev)))))


(provide 'init-haskell)