;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-

;; tried themes: doom-themes, color-theme-sanityinc-tomorrow, sanityinc-tomorrow-night, zeno-theme, moe-theme, cloud-theme, kaolin-theme, gruber-darker-theme, nimbus-theme, leuven-theme
(use-package doom-themes  
  :ensure t
  :defer t)

(use-package weyland-yutani-theme
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package kaolin-themes
  :ensure t
  :defer t)

(use-package moe-theme
  :ensure t
  :defer t)

;; ===set default theme
(setq-default custom-enabled-theme 'doom-dracula)
(unless (display-graphic-p)
  (setq custom-enabled-theme 'wombat))


;; load theme and do customization for themes
(defun zw/load-theme ()
  (interactive)
  ;; load functions which does customization
  (require 'init-themes-customization)
  
  (load-theme custom-enabled-theme)
  
  (zw/customize-themes-for-general)
  (zw/customize-themes-for-parenthesis)
  ;; this helm setting effect a lot
  (zw/customize-pkg-with-fn 'helm 'zw/customize-themes-for-helm)

  ;; TODO: why zw/customize-pkg-with-fn doesn't work for scenarios need after-load
  (defun zw/customize-package-attribute-for-theme (package-name customize-fn)
    (if (featurep package-name)
        (funcall customize-fn)
      (after-load package-name
        (funcall customize-fn))))
  (zw/customize-package-attribute-for-theme 'selectrum 'zw/customize-themes-for-selectrum)
  (zw/customize-package-attribute-for-theme 'symbol-overlay 'zw/customize-themes-for-symbol-overlay)
  (zw/customize-package-attribute-for-theme 'indent-guide 'zw/customize-themes-for-indent-guide)
  (zw/customize-package-attribute-for-theme 'org 'zw/customize-themes-for-org)
  (zw/customize-package-attribute-for-theme 'company 'zw/customize-themes-for-company)
  (zw/customize-package-attribute-for-theme 'ivy 'zw/customize-themes-for-ivy)
  (zw/customize-package-attribute-for-theme 'dashboard 'zw/customize-themes-for-dashboard)
  
  (add-hook 'smartparens-mode-hook 'zw/customize-themes-for-smartparens)
  ;; Finally, adjust some appearence based on current theme 
  (zw/customize-themes-for-particular-one))


;; Don't prompt to confirm theme safety. This 
(setq custom-safe-themes t)

;; remember to install https://github.com/domtronn/all-the-icons.el
(setq doom-themes-treemacs-theme "doom-colors")
(doom-themes-treemacs-config)

(add-hook 'after-init-hook 'zw/load-theme)


(provide 'init-themes)
;;; init-themes.el ends here
