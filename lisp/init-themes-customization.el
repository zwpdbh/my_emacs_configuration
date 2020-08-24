
(when (string-equal custom-enabled-theme "doom-Iosvkem")
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


(provide 'init-themes-customization)