;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-

;; tried themes: doom-themes, color-theme-sanityinc-tomorrow, sanityinc-tomorrow-night, zeno-theme, moe-theme, cloud-theme, kaolin-theme, gruber-darker-theme, nimbus-theme, leuven-theme

(use-package doom-themes
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package leuven-theme
  :ensure t
  :defer t)

(use-package weyland-yutani-theme
  :ensure t
  :defer t)


;; set default theme
;; sanityinc-tomorrow-night
;; sanityinc-tomorrow-bright
;; gruber-darker

(setq-default custom-enabled-theme 'weyland-yutani)

(use-package symbol-overlay
  :ensure t
  :config
  (set-face-attribute 'symbol-overlay-default-face nil
                      :inherit nil
                      :foreground (face-foreground 'default t t)
                      :background (face-background 'default t t)
                      :underline t))


;; Ensure that themes will be applied even if they have not been customized
(defun apply-theme ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (setq custom-safe-themes t) ; Don't prompt to confirm theme safety. This 
  
  ;; remember to install https://github.com/domtronn/all-the-icons.el
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (load-theme custom-enabled-theme))
(apply-theme)

(cond ((eql custom-enabled-theme 'weyland-yutani)
       (message "customize weyland-yutani theme")
       (setq show-paren-style 'expression)
       (set-face-attribute 'show-paren-match nil
                           :weight 'normal
                           :underline nil
                           :foreground (face-background 'default t t)
                           :background "black"))
      ((eql custom-enabled-theme 'leuven)
       (zw/customize-general-leuven-theme)
       (add-hook 'company-mode-hook 'zw/customize-leuven-theme-for-company)
       (add-hook 'helm-mode-hook 'zw/customize-leuven-theme-for-helm)
       (add-hook 'org-mode-hook 'zw/customize-leuven-theme-for-org)
       (add-hook 'ivy-mode-hook 'zw/customize-leuven-theme-for-swiper)
       (add-hook 'doom-modeline-mode-hook 'zw/customize-leuven-theme-for-doom-modeline)))

(provide 'init-themes)
;;; init-themes.el ends here
