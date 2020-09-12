
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


;; ===== make window/buffer move easier 
(when (maybe-require-package 'buffer-move)
  (add-hook 'after-init-hook '(lambda ()
                                (global-set-key (kbd "C-x C-<up>") 'buf-move-up)
                                (global-set-key (kbd "C-x C-<left>") 'buf-move-left)
                                (global-set-key (kbd "C-x C-<right>") 'buf-move-right)
                                (global-set-key (kbd "C-x C-<down>") 'buf-move-down))))

;; ===== set mode-line 
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


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


(provide 'init-convenient)
