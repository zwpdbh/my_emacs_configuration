
;; ===== set buffer and shell 
(use-package exec-path-from-shell
  :defer 2
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
(use-package buffer-move
  :ensure t 
  :defer t)


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

;; sudo edit
(use-package sudo-edit
  :ensure t
  :defer 1)

(provide 'init-convenient)
