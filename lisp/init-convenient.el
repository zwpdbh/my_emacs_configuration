
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
  (global-unset-key (kbd "<M-S-up>"))
  (global-set-key (kbd "M-S-<up>") 'buf-move-up)
  (global-set-key (kbd "M-S-<left>") 'buf-move-left)
  (global-set-key (kbd "M-S-<right>") 'buf-move-right)
  (global-set-key (kbd "M-S-<down>") 'buf-move-down))


;; ===== set mode-line 
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


;; ===== install all the icons 
(use-package all-the-icons
  :ensure t 
  :defer t)
;; After the package installed, run ~M-x all-the-icons-install-fonts~
;; For Windows10, after executed the above command, go to the place specified to manually install theme.

;; ===== magit 
(use-package magit
  :bind (("C-x g" . magit))
  :ensure t)

(use-package expand-region
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-=") 'er/expand-region)
    (global-set-key (kbd "C--") 'er/contract-region)))


;; show a cat in modeline
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1)
  (setq nyan-cat-face-number 3))

(provide 'init-convenient)
