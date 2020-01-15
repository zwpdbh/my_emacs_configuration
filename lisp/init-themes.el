;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-

;; (use-package doom-themes
;;   :init
;;   (setq custom-safe-themes t)
;;   (setq doom-themes-enable-bold nil
;;         doom-themes-enable-italic nil)
;;   :ensure t
;;   :config
;;   (progn
;;     (load-theme 'doom-tomorrow-night t)

;;     ;; use the colorful treemacs theme
;;     (setq doom-themes-treemacs-theme "doom-colors")
;;     ;; (doom-themes-treemacs-config)

;;     ;; Corrects (and improves) org-mode's native fontification.
;;     ;; (doom-themes-org-config)

;;     (set-cursor-color "HotPink")
;;     (setq-default cursor-type '(bar . 2))
;;     (set-face-attribute 'show-paren-match nil
;;                         :underline t
;;                         :foreground "SteelBlue"
;;                         :background nil
;;                         :weight 'ultrabold)))

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (setq custom-safe-themes t)
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes)))
  
  (set-cursor-color "HotPink")
  (setq-default cursor-type '(bar . 2))
  (set-face-attribute 'show-paren-match nil
                      :underline t
                      :foreground "SteelBlue";; "#D4FF00"
                      :background nil
                      :weight 'ultrabold))

(add-hook 'after-init-hook 'reapply-themes)

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

(provide 'init-themes)
;;; init-themes.el ends here
