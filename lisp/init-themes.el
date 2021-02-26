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
    (setq custom-enabled-theme 'doom-oceanic-next)
  (setq custom-enabled-theme 'doom-Iosvkem))


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
  
  (if (featurep 'dashboard)
      (funcall 'zw/customize-themes-for-dashboard)
    (after-load 'dashboard
      (funcall 'zw/customize-themes-for-dashboard)))
  
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
