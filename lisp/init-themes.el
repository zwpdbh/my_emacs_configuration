;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-

;; tried themes: doom-themes, color-theme-sanityinc-tomorrow, sanityinc-tomorrow-night, zeno-theme, moe-theme, cloud-theme, kaolin-theme, gruber-darker-theme, nimbus-theme, leuven-theme

(use-package doom-themes
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package leuven-theme
  :ensure t
  :defer t)

(use-package weyland-yutani-theme
  :ensure t
  :defer t)

(use-package noctilux-theme
  :ensure t
  :defer)

;; set default theme
;; sanityinc-tomorrow-night
;; sanityinc-tomorrow-bright
;; gruber-darker

(setq-default custom-enabled-theme 'weyland-yutani)
(if (display-graphic-p)
    (setq custom-enabled-theme 'weyland-yutani)
  (setq custom-enabled-theme 'doom-Iosvkem))


(defun customize-for-leuven-theme ()
  (require 'init-themes-customization-for-leuven)
  (add-hook 'after-init-hook
            '(lambda ()
               (zw/customize-general-leuven-theme)
               (zw/customize-leuven-theme-for-company)
               (zw/customize-leuven-theme-for-helm)
               (zw/customize-leuven-theme-for-org)
               (zw/customize-leuven-theme-for-swiper)
               (zw/customize-leuven-theme-for-ivy)
               (zw/customize-leuven-theme-for-modeline)
               (zw/customize-leuven-theme-for-symbol-overlay))))

(defun customize-for-weyland-theme ()
  (require 'init-themes-customization-for-weyland)
  (add-hook 'after-init-hook
            '(lambda ()
               (zw/customize-general-weyland-theme)
               (zw/customize-weyland-theme-for-helm)
               (zw/customize-weyland-theme-for-ivy)
               ;; (zw/customize-weyland-theme-for-swiper)
               (zw/customize-weyland-theme-for-org)
               (zw/customize-weyland-theme-for-symbol-overlay)
               (zw/customize-weyland-theme-for-js2)
               (zw/customize-weyland-theme-for-indent-guide)))
  (add-hook 'web-mode-hook 'zw/customize-weyland-theme-for-web-mode)
  (add-hook 'smartparens-mode-hook 'zw/customize-weyland-theme-for-smartparens)
  (add-hook 'flycheck-mode-hook 'zw/customize-weyland-theme-for-flycheck)
  
  (zw/customize-weyland-theme-for-selectrum)
  (zw/customize-weyland-theme-for-marginalia)
  (zw/customize-weyland-theme-for-consult)
  (zw/customize-weyland-theme-for-orderless))


;; Ensure that themes will be applied even if they have not been customized
(defun zw/apply-theme ()
  (interactive)
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (setq custom-safe-themes t) ; Don't prompt to confirm theme safety. This 
  
  ;; remember to install https://github.com/domtronn/all-the-icons.el
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (load-theme custom-enabled-theme)

  ;; customize themes based on current theme
  (cond ((eql custom-enabled-theme 'weyland-yutani)
         (customize-for-weyland-theme))
        ((eql custom-enabled-theme 'leuven)
         (customize-for-leuven-theme))))


(zw/apply-theme)


(provide 'init-themes)
;;; init-themes.el ends here
