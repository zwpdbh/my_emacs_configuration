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
      zw/blue-for-org-code-in-leuven "#336699"
      zw/light-purple "#ccccff"
      zw/white "#def")


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
                      :background zw/light-purple)

  ;; make helm related selection use underline
  (set-face-attribute 'helm-selection nil
                      :underline t
                      :weight 'bold)

  ;; make swiper use code default color as foreground
  (set-face-attribute 'isearch nil
                      :foreground (face-foreground 'default t t)
                      :weight 'normal)

  (cond
   ;; for theme leuven
   ((string-equal custom-enabled-theme "leuven")
    (set-background-color "honeydew")
    ;; for Border/frame around Emacs frame
    (set-face-attribute 'fringe nil
                        :foreground "#4C9ED9"
                        :background (face-attribute 'default :background))
    (set-face-attribute 'show-paren-match nil
                        :weight 'normal
                        :underline nil
                        :foreground (face-background 'default t t)
                        :background zw/light-purple)

    (set-face-attribute 'helm-match-item nil
                        :weight 'bold
                        :background "#FFFF00"
                        :underline nil
                        :extend t)
    (set-face-attribute 'swiper-match-face-2 nil
                        :weight 'bold
                        :background "#FFFF00"
                        :underline nil
                        :extend t)

    (set-face-attribute 'doom-modeline-project-dir nil
                        :weight 'bold
                        :foreground "#99cc00")
    (set-face-attribute 'doom-modeline-buffer-modified nil
                        :weight 'bold
                        :foreground "#ffcc00")
    
    (add-hook 'org-mode-hook '(lambda ()
                                (set-face-attribute 'org-code nil
                                                    :foreground zw/blue-for-org-code-in-leuven)
                                (setq org-emphasis-alist
                                      '(("*" (bold :foreground "#336699"))
                                        ("/" italic)
                                        ("_" underline)
                                        ("=" org-verbatim verbatim)
                                        ("~" org-code verbatim)
                                        ("+" (:strike-through t))))))
    (set-face-attribute 'region nil
                        :background "#cce6ff"
                        :extend t))
   
   ;; for theme: doom-Iosvkem
   ((string-equal custom-enabled-theme "doom-Iosvkem")
    (add-hook 'company-mode-hook
              '(lambda ()
                 (set-face-attribute 'company-tooltip-common nil
                                     :foreground zw/green
                                     :weight 'bold)
                 (set-face-attribute 'company-preview-common nil
                                     :foreground zw/green
                                     :background (face-background 'default t t))))

    (set-face-attribute 'link nil
                        :foreground zw/green
                        :weight 'bold
                        :underline t)
    (set-face-attribute 'highlight nil
                        :background zw/red)

    (set-face-attribute 'show-paren-match nil
                        :weight 'normal
                        :underline nil
                        :foreground (face-background 'default t t)
                        :background "black"))
   ;; for other themes
   (t
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
                                (setq org-emphasis-alist
                                      '(("*" (bold :foreground "Gold"))
                                        ("/" italic)
                                        ("_" underline)
                                        ("=" org-verbatim verbatim)
                                        ("~" org-code verbatim)
                                        ("+" (:strike-through t))))))

    (add-hook 'org-mode-hook '(lambda ()
                                (set-face-attribute 'org-block nil
                                                    :foreground (face-foreground 'default t t)))))))

(provide 'init-themes-customization)