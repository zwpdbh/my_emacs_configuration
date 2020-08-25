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

  (cond
   ;; for theme leuven
   ((string-equal custom-enabled-theme "leuven")
    (set-background-color "honeydew")
    (set-face-attribute 'show-paren-match nil
                        :weight 'normal
                        :underline nil
                        :foreground (face-background 'default t t)
                        :background zw/light-purple)

    (add-hook 'org-mode-hook '(lambda ()
                                (set-face-attribute 'org-code nil
                                                    :foreground "#336699")
                                (setq org-emphasis-alist
                                      '(("*" (bold :foreground "Gold"))
                                        ("/" italic)
                                        ("_" underline)
                                        ("=" org-verbatim verbatim)
                                        ("~" org-code verbatim)
                                        ("+" (:strike-through t)))))))
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
                                (set-face-attribute 'org-block nil
                                                    :foreground (face-foreground 'default t t))
                                (setq org-emphasis-alist
                                      '(("*" (bold :foreground "Gold"))
                                        ("/" italic)
                                        ("_" underline)
                                        ("=" org-verbatim verbatim)
                                        ("~" org-code verbatim)
                                        ("+" (:strike-through t)))))))))

(provide 'init-themes-customization)