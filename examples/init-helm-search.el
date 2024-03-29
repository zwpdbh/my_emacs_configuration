(when (executable-find "rg")
  ;; ===sudo apt install ripgrep
  (when (maybe-require-package 'rg)
    (when (maybe-require-package 'helm-rg)      
      (setq helm-rg-ripgrep-executable (executable-find "rg"))
      (setq helm-rg-default-directory 'git-root)
      ;; from https://github.com/BurntSushi/ripgrep
      (defconst modi/rg-arguments
        `("--line-number"                     ; line numbers
          "--smart-case"
          "--follow"                          ; follow symlinks
          "--mmap")                           ; apply memory map optimization when possible
        "Default rg arguments used in the functions in `projectile' package.")
      (defun zw/advice-projectile-use-rg ()
        "Always use `rg' for getting a list of all files in the project."
        (mapconcat 'identity
                   (append '("\\rg") ; used unaliased version of `rg': \rg
                           modi/rg-arguments
                           '("--null" ; output null separated results,
                             "--files")) ; get file names matching the regex '' (all files)
                   " "))
      (advice-add 'projectile-get-ext-command :override #'zw/advice-projectile-use-rg))))

;; === brew install the_silver_searcher
;; === choco install ag
;; === apt-get install silversearcher-ag
(use-package ag
  :ensure t
  :defer t)

;; available options see: https://github.com/emacsorphanage/helm-ag
(when (maybe-require-package 'helm-ag)
  (setq helm-ag-command-option "--hidden --ignore .git"
        helm-ag-insert-at-point nil))


(provide 'init-helm-search)
