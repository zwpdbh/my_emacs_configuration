;; references
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md#using-emacs-for-haskell-development

(when (maybe-require-package 'haskell-mode)
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))

  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t)

  (setq haskell-process-type 'cabal-repl)
  
  ;; Or use current-local-map: (define-key (current-local-map) (kbd "M-.") '<functional-to-call>)
  (after-load 'haskell-mode
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
    (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))

  (after-load 'haskell-cabal
    (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
    (define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile)))

(provide 'init-haskell)