
;; ===== install all the icons 
(use-package all-the-icons
  :ensure t 
  :defer t)
;; After the package installed, run ~M-x all-the-icons-install-fonts~
;; For Windows10, after executed the above command, go to the place specified to manually install theme.


(use-package all-the-icons-ibuffer
  :ensure t
  :init (when (display-graphic-p)
          (all-the-icons-ibuffer-mode 1))
  :config
  ;; The default icon size in ibuffer.
  (setq all-the-icons-ibuffer-icon-size 1.0)

  ;; The default vertical adjustment of the icon in ibuffer.
  (setq all-the-icons-ibuffer-icon-v-adjust 0.0)

  ;; Use human readable file size in ibuffer.
  (setq  all-the-icons-ibuffer-human-readable-size t)

  ;; A list of ways to display buffer lines with `all-the-icons'.
  ;; See `ibuffer-formats' for details. all-the-icons-ibuffer-formats

  ;; Slow Rendering
  ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
  ;; you can try setting the following variable
  (setq inhibit-compacting-font-caches t))


(provide 'init-all-the-icons)