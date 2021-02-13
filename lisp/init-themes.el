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
    (setq custom-enabled-theme 'doom-vibrant)
  (setq custom-enabled-theme 'doom-Iosvkem))

;;; previous configurations
;; (defun customize-for-weyland-theme ()
;;   (require 'init-themes-customization-for-weyland)
;;   (add-hook 'after-init-hook
;;             '(lambda ()
;;                (zw/customize-general-weyland-theme)
;;                (zw/customize-weyland-theme-for-helm)
;;                (zw/customize-weyland-theme-for-ivy)
;;                ;; (zw/customize-weyland-theme-for-swiper)
;;                (zw/customize-weyland-theme-for-js2)
;;                (zw/customize-weyland-theme-for-indent-guide)))

;;   (add-hook 'org-mode-hook 'zw/customize-weyland-theme-for-org)
;;   (add-hook 'symbol-overlay-mode-hook 'zw/customize-weyland-theme-for-symbol-overlay)
;;   (add-hook 'web-mode-hook 'zw/customize-weyland-theme-for-web-mode)
;;   (add-hook 'smartparens-mode-hook 'zw/customize-weyland-theme-for-smartparens)
;;   (add-hook 'flycheck-mode-hook 'zw/customize-weyland-theme-for-flycheck)

;;   (zw/customize-weyland-theme-for-selectrum)
;;   (zw/customize-weyland-theme-for-marginalia)
;;   (zw/customize-weyland-theme-for-consult)
;;   (zw/customize-weyland-theme-for-orderless)

;;   (zw/customize-weyland-theme-for-company)
;;   (zw/customize-weyland-theme-for-magit))


;; load theme and do customization for themes
(defun zw/load-themes ()
  (interactive)
  (require 'init-themes-customization)
  
  (load-theme custom-enabled-theme)
  (zw/customize-themes-for-parenthesis)
  (zw/customize-pkg-with-fn 'helm 'zw/customize-themes-for-helm)
  
  ;; TODO: why zw/customize-pkg-with-fn doesn't work for scenarios need after-load
  (if (featurep 'selectrum)
      (funcall 'zw/customize-themes-for-selectrum)
    (after-load 'selectrum
      (funcall 'zw/customize-themes-for-selectrum)))
  
  (if (featurep 'symbol-overlay)
      (funcall 'zw/customize-themes-for-symbol-overlay)
    (after-load 'symbol-overlay
      (funcall 'zw/customize-themes-for-symbol-overlay)))

  (if (featurep 'indent-guide)
      (funcall 'zw/customize-themes-for-indent-guide)
    (after-load 'indent-guide
      (funcall 'zw/customize-themes-for-indent-guide)))
  (if (featurep 'org)
      (funcall 'zw/customize-themes-for-org)
    (after-load 'org
      (funcall 'zw/customize-themes-for-org))))


;; Don't prompt to confirm theme safety. This 
(setq custom-safe-themes t)

;; remember to install https://github.com/domtronn/all-the-icons.el
(setq doom-themes-treemacs-theme "doom-colors")
(doom-themes-treemacs-config)


(add-hook 'after-init-hook 'zw/load-themes)


(provide 'init-themes)
;;; init-themes.el ends here
