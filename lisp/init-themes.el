;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require-package 'color-theme-sanityinc-solarized)
;; (require-package 'color-theme-sanityinc-tomorrow)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  :ensure t
  :config
  (progn
    (load-theme 'doom-vibrant t)
    (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
    ;; (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    ;; (doom-themes-org-config)
    (set-cursor-color "HotPink")
    (setq-default cursor-type '(bar . 2))))

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)


;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;; If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(doom-vibrant))
;; (add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(doom-one-light))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(doom-vibrant))
  (reapply-themes))

(provide 'init-themes)
;;; init-themes.el ends here
