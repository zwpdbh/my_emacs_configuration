;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-

;; tried themes: doom-themes, color-theme-sanityinc-tomorrow, sanityinc-tomorrow-night, zeno-theme, moe-theme, cloud-theme, kaolin-theme, gruber-darker-theme, nimbus-theme, leuven-theme
(use-package doom-themes
  :ensure t
  :defer t)

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :defer t)

;; set default theme
;; sanityinc-tomorrow-night
;; sanityinc-tomorrow-bright
;; gruber-darker

(setq-default custom-enabled-theme 'doom-palenight)

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

  (set-cursor-color "IndianRed")
  (setq-default cursor-type '(bar . 2))
  ;; (setq-default cursor-type 'hollow)
  
  (setq show-paren-style 'expression)
  ;; tried color candidates: SteelBlue
  ;; #73c936 green
  ;; #b294bb purple
  ;; #f0c674 yellow
  ;; "IndianRed"
  ;; "MediumPurple4"
  ;; "#def"  Font Lock Keyword  Face 
  (set-face-attribute 'show-paren-match nil
                      :weight 'normal
                      :underline nil
                      :foreground (face-background 'default t t)
                      :background "dark slate blue")

  ;; make helm related selection use underline
  (set-face-attribute 'helm-selection nil
                      :underline t
                      :weight 'bold)

  ;; make swiper use code default color as foreground
  (set-face-attribute 'isearch nil
                      :foreground (face-foreground 'default t t)
                      :weight 'normal)
  
  ;; make swiper selection use underline
  (add-hook 'ivy-mode-hook
            '(lambda ()
               (set-face-attribute 'ivy-current-match nil
                :inherit nil
                :foreground nil
                :background nil
                :underline t
                :weight 'bold)))

  ;; set company selection highlight
  (add-hook 'company-mode-hook
            '(lambda ()
               (set-face-attribute 'company-tooltip-selection nil
                :foreground (face-foreground 'default t t)
                :background "black"
                :inverse-video nil
                :weight 'extrabold)))

  ;; set the lsp-ui-peek related
  (add-hook 'lsp-ui-mode-hook
            '(lambda ()
               (set-face-attribute 'lsp-ui-peek-selection nil
                :foreground (face-foreground 'hl-line t t)
                :background (face-background 'hl-line nil t)
                :underline nil
                :weight 'bold)
               
               (set-face-attribute 'lsp-ui-peek-highlight nil
                :foreground "Yellow"
                :background (face-background 'default t t)
                :underline nil
                :box nil
                :weight 'normal)
               (set-face-attribute 'lsp-ui-peek-peek nil
                :foreground (face-foreground 'default t t)
                :background (face-background 'default t t))))
  
  ;; set different org-mode color
  (add-hook 'org-mode-hook '(lambda ()
                              (set-face-attribute 'org-code nil
                               :foreground "#73c936")
                              (set-face-attribute 'org-block nil
                               :foreground (face-foreground 'default t t))
                              (setq org-emphasis-alist
                               '(("*" (bold :foreground "Gold"))
                                 ("/" italic)
                                 ("_" underline)
                                 ("=" org-verbatim verbatim)
                                 ("~" org-code verbatim)
                                 ("+" (:strike-through t)))))))


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
