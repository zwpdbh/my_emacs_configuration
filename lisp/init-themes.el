;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-

(use-package doom-themes
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package zeno-theme
  :ensure t
  :defer t)

(use-package zerodark-theme
  :ensure t
  :defer t)

(use-package moe-theme
  :ensure t
  :defer t)

(use-package cloud-theme
  :ensure t
  :defer t)

(use-package kaolin-themes
  :ensure t
  :defer t)

(use-package nimbus-theme
  :ensure t
  :defer t)

;; set default theme
;; sanityinc-tomorrow-night
;; sanityinc-tomorrow-bright
(setq-default custom-enabled-theme 'moe-light)

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (setq custom-safe-themes t) ; Don't prompt to confirm theme safety. This 
  (load-theme custom-enabled-theme)
  
  (set-cursor-color "HotPink")
  (setq-default cursor-type '(bar . 2))
  
  (set-face-attribute 'show-paren-match nil
                      :underline nil
                      :foreground "SteelBlue" ;; use Font Lock Keyword Face 
                      :background nil
                      :weight 'normal)
  
  (setq show-paren-style 'expression)
  
  ;; remember to install https://github.com/domtronn/all-the-icons.el
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(add-hook 'after-init-hook 'reapply-themes)
;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun zw/theme-sanityinc-tomorrow-bright ()
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

(defun zw/theme-leuven ()
  (interactive)
  (setq custom-enabled-themes '(leuven))
  (reapply-themes))

(provide 'init-themes)
;;; init-themes.el ends here
