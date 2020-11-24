(defun zw/customize-weyland-theme-for-helm ()
  ;; make helm related selection use underline
  (set-face-attribute 'helm-selection nil
                      :underline t
                      :foreground (face-foreground 'default t t)
                      :weight 'bold
                      :extend t)
  (setq helm-buffer-max-length 36)

  (set-face-attribute 'helm-ff-directory nil
                      :foreground "#20b2aa"
                      :background (face-background 'default t t)
                      :weight 'bold)
  (set-face-attribute 'helm-ff-dotted-directory nil
                      :foreground "#20b2aa"
                      :background (face-background 'default t t)
                      :weight 'bold))

(defun zw/customize-general-weyland-theme ()
  (message "customize weyland-yutani theme")
  (setq show-paren-style 'expression)
  (set-face-attribute 'show-paren-match nil
                      :weight 'normal
                      :underline nil
                      :foreground (face-background 'default t t)
                      :background "black")
  ;; make weyland theme use normal font size for header-line
  (set-face-attribute 'header-line nil
                      :height 1.0
                      :underline nil)
  ;; disable fringe 
  (set-fringe-mode 0))

(defun zw/customize-weyland-theme-for-swiper ()
  ;; make selection highlight-background expand full width of the minibuffer
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line))


(defun zw/customize-weyland-theme-for-org ()
  (setq org-emphasis-alist
        '(("*" (:foreground "gold"))
          ("/" italic)
          ("_" underline)
          ("=" (:background "maroon" :foreground "white"))
          ("~" (:foreground "#fb8a69"))
          ("+" (:Strike-through t)))))

(defun zw/customize-weyland-theme-for-symbol-overlay ()
  (add-hook 'symbol-overlay-mode-hook
            '(lambda ()
               (set-face-attribute 'symbol-overlay-default-face nil
                                   :inherit nil
                                   :weight 'normal
                                   :foreground (face-foreground 'default t t)
                                   :background "#191970"
                                   ;; :underline '(:color "gray")
                                   :underline t))))

(defun zw/customize-weyland-theme-for-js2 ()
  (add-hook 'js2-mode-hook
            '(lambda ()
               (set-face-attribute 'js2-error nil
                                   :foreground "tomato"))))

(provide 'init-themes-customization-for-weyland)