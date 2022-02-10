(setq coding-system-for-write 'utf-8-unix)
;; ===== set buffer and shell 
(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))


;; ===== try 
(use-package try
  :commands (try)
  :ensure t)

;; ===== set mode-line
(when *all-the-icons-installed-p*
  (use-package doom-modeline
    :ensure t
    :init (setq doom-modeline-vcs-max-length 20)
    :hook (after-init . doom-modeline-mode)))


(use-package expand-region
  :ensure t
  :config
  (progn
    (global-set-key (kbd "M-=") 'er/expand-region)
    (global-set-key (kbd "M--") 'er/contract-region)))


;; Instant scratch buffer for current mode
;; https://github.com/ieure/scratch-el
(add-to-list 'load-path
             "~/.emacs.d/site-lisp/scratch-el")
;; uses package "scratch"
(autoload 'scratch "scratch" nil t)
;; M-x scratch, Immediately create a scratch buffer with the same major mode as the current buffer’s.
;; C-u M-x scratch, Prompts for a major mode to create a scratch buffer with.


(provide 'init-convenient)
