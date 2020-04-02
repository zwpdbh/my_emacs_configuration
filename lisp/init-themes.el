;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-

;; tried themes: doom-themes, color-theme-sanityinc-tomorrow, zeno-theme, moe-theme, cloud-theme, kaolin-theme, gruber-darker-theme, nimbus-theme, leuven-theme
(use-package doom-themes
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package gruber-darker-theme
  :ensure t
  :defer t)

;; set default theme
;; sanityinc-tomorrow-night
;; sanityinc-tomorrow-bright
;; gruber-darker
;; (setq-default custom-enabled-theme 'gruber-darker)
(setq-default custom-enabled-theme 'sanityinc-tomorrow-night)

(use-package symbol-overlay
  :ensure t
  :config
  (set-face-attribute 'symbol-overlay-default-face nil
                      :inherit 'highlight
                      :underline t))

(defun zw/customize-theme ()
  (load-theme custom-enabled-theme)

  (set-cursor-color "IndianRed")
  (setq-default cursor-type '(bar . 2))
  ;; (setq-default cursor-type 'hollow)
  
  (setq show-paren-style 'expression)
  ;; tried color candidates: SteelBlue
  ;; #c397d8 purple
  ;; #73c936 green
  ;; "IndianRed"
  (set-face-attribute 'show-paren-match nil
                      :underline nil
                      :foreground "#73c936"  ;; use Font Lock Keyword  Face 
                      :background "black"
                      :weight 'normal)
  
  
  ;; make helm related selection use underline
  (set-face-attribute 'helm-selection nil
                      :underline t)
  
  ;; set different org-mode color
  (add-hook 'org-mode-hook '(lambda ()
                              (set-face-attribute 'org-code nil
                                                  :foreground "#73c936")
                              (set-face-attribute 'org-block nil
                                                  :foreground "default"))))


;; Ensure that themes will be applied even if they have not been customized
(defun apply-theme ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (setq custom-safe-themes t) ; Don't prompt to confirm theme safety. This 
  
  ;; remember to install https://github.com/domtronn/all-the-icons.el
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  
  (zw/customize-theme))

(add-hook 'after-init-hook 'apply-theme)

(provide 'init-themes)
;;; init-themes.el ends here
