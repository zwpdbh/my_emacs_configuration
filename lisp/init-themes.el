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

(use-package doom-themes
  :ensure t
  :defer t
  :init
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package cloud-theme
  :ensure t 
  :defer t)

(use-package leuven-theme
  :ensure t
  :defer t)

;; set default theme
(setq-default custom-enabled-themes '(doom-tomorrow-night))
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (setq custom-safe-themes t) ; Don't prompt to confirm theme safety. This avoids problems with
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
(defun zw/theme-sanityinc-tomorrow-bright ()
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

(defun zw/theme-cloud ()
  (interactive)
  (setq custom-enabled-themes '(cloud))
  (reapply-themes))

(defun zw/theme-leuven ()
  (interactive)
  (setq custom-enabled-themes '(leuven))
  (reapply-themes))

(defun zw/doom-tomorrow-night ()
  (interactive)
  (setq custom-enabled-themes '(doom-tomorrow-night))
  (reapply-themes))


(provide 'init-themes)
;;; init-themes.el ends here
