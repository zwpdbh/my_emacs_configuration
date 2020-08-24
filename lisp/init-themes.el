;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-

;; tried themes: doom-themes, color-theme-sanityinc-tomorrow, sanityinc-tomorrow-night, zeno-theme, moe-theme, cloud-theme, kaolin-theme, gruber-darker-theme, nimbus-theme, leuven-theme

(defvar zw/green)
(defvar zw/purple)
(defvar zw/yellow)
(defvar zw/red)
(defvar zw/blue-purple)
(defvar zw/white)

(setq zw/green "#73c936"
      zw/purple "#b294bb"
      zw/yellow "#f0c674"
      zw/red  "IndianRed"
      zw/blue-purple "#352d67"
      zw/white "#def")


(use-package doom-themes
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t)


;; set default theme
;; sanityinc-tomorrow-night
;; sanityinc-tomorrow-bright
;; gruber-darker

(setq-default custom-enabled-theme 'zenburn)

(use-package symbol-overlay
  :ensure t
  :config
  (set-face-attribute 'symbol-overlay-default-face nil
                      :inherit nil
                      :foreground (face-foreground 'default t t)
                      :background (face-background 'default t t)
                      :underline t))


(defun zw/customize-theme ()
  (load-theme custom-enabled-theme)

  (when window-system
    (set-cursor-color zw/red)
    (setq-default cursor-type '(bar . 2)))

  (when window-system
    (setq show-paren-style 'expression))

  (set-face-attribute 'show-paren-match nil
                      :weight 'normal
                      :underline nil
                      :foreground (face-background 'default t t)
                      :background zw/blue-purple)

  ;; make helm related selection use underline
  (set-face-attribute 'helm-selection nil
                      :underline t
                      :weight 'bold)

  ;; make swiper use code default color as foreground
  (set-face-attribute 'isearch nil
                      :foreground (face-foreground 'default t t)
                      :weight 'normal)
  

  ;; set company selection highlight
  (add-hook 'company-mode-hook
            '(lambda ()
               (set-face-attribute 'company-tooltip-selection nil
                                   :foreground (face-foreground 'default t t)
                                   :background "black"
                                   :inverse-video nil
                                   :weight 'extrabold)))

  
  ;; set different org-mode color
  (add-hook 'org-mode-hook '(lambda ()
                              (set-face-attribute 'org-code nil
                                                  :foreground zw/green)
                              (set-face-attribute 'org-block nil
                                                  :foreground (face-foreground 'default t t))
                              (setq org-emphasis-alist
                                    '(("*" (bold :foreground "Gold"))
                                      ("/" italic)
                                      ("_" underline)
                                      ("=" org-verbatim verbatim)
                                      ("~" org-code verbatim)
                                      ("+" (:strike-through t)))))))

;; make swiper selection use underline
(defun zw/customize-ivy-current-match ()
  (interactive)
  (set-face-attribute 'ivy-current-match nil
                      :inherit nil
                      :foreground nil
                      :background nil
                      :underline t
                      :weight 'bold))
;; (add-hook 'ivy-mode-hook #'zw/customize-ivy-current-match)


;; Ensure that themes will be applied even if they have not been customized
(defun apply-theme ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (setq custom-safe-themes t) ; Don't prompt to confirm theme safety. This 
  
  ;; remember to install https://github.com/domtronn/all-the-icons.el
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  
  (zw/customize-theme)
  (zw/customize-ivy-current-match))

(add-hook 'after-init-hook 'apply-theme)

(provide 'init-themes)
;;; init-themes.el ends here
