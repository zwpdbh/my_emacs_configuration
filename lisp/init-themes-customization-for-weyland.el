(defun zw/customize-weyland-theme-for-helm ()
  ;; make helm related selection use underline
  (set-face-attribute 'helm-selection nil
                      :underline t
                      :foreground (face-foreground 'default t t)
                      :weight 'bold
                      :extend t)
  (setq helm-buffer-max-length 36)

  (set-face-attribute 'helm-ff-directory nil
                      :foreground "#67bfba"
                      :background (face-background 'default t t)
                      :weight 'bold)
  (set-face-attribute 'helm-ff-dotted-directory nil
                      :foreground "#20b2aa"
                      :background (face-background 'default t t)
                      :weight 'bold))

(defun zw/customize-general-weyland-theme ()
  (message "customize weyland-yutani theme")
  (when window-system
    (set-cursor-color zw/red)
    (setq-default cursor-type '(bar . 3)))
  (setq show-paren-style 'expression)
  (set-face-attribute 'show-paren-match nil
                      :weight 'normal
                      :underline nil
                      :foreground (face-foreground 'default t t)
                      :background "black")
  ;; make weyland theme use normal font size for header-line
  (set-face-attribute 'header-line nil
                      :height 1.0
                      :underline nil)

  ;; set highlight face
  (set-face-attribute 'highlight nil
                      :weight 'bold
                      :foreground "#86dc2f"
                      :background (face-background 'default t t))
  
  ;; disable fringe 
  (set-fringe-mode 0))


(defun zw/customize-weyland-theme-for-ivy ()
  ;; make selection highlight-background expand full width of the minibuffer
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)

  ;; make ivy current match background lighter
  (set-face-attribute 'ivy-current-match nil
                      :weight 'bold
                      :foreground (face-background 'default t t)
                      :background "#3b3559")
  
  ;; Face for ‘ivy’ minibuffer matches covered by inputs
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil
                      :weight 'normal
                      :underline t
                      :background (face-background 'default t t))
  
  ;; Face for ‘ivy’ minibuffer matches inputs 
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                      :weight 'bold
                      :underline t
                      :background (face-background 'default t t)))


(defun zw/customize-weyland-theme-for-swiper ()
  (set-face-attribute 'swiper-line-face nil
                      :inherit nil
                      :weight 'bold
                      :underline nil
                      :foreground (face-foreground 'default t t)
                      :background "#3b3559")
  
  (set-face-attribute 'swiper-match-face-1 nil
                      :inherit nil
                      :weight 'normal
                      :underline t
                      :foreground (face-foreground 'default t t)
                      :background nil)

  (set-face-attribute 'swiper-match-face-2 nil
                      :inherit nil
                      :weight 'normal
                      :underline t
                      :foreground (face-foreground 'default t t)
                      :background nil)

  (set-face-attribute 'swiper-match-face-3 nil
                      :inherit nil
                      :weight 'normal
                      :underline t
                      :foreground (face-foreground 'default t t)
                      :background nil)

  (set-face-attribute 'swiper-match-face-4 nil
                      :inherit nil
                      :weight 'normal
                      :underline t
                      :foreground (face-foreground 'default t t)
                      :background nil))

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
                                   :weight 'normal
                                   :foreground (face-foreground 'default t t)
                                   :background nil
                                   :underline t
                                   :inherit nil))))

(defun zw/customize-weyland-theme-for-js2 ()
  (add-hook 'js2-mode-hook
            '(lambda ()
               (set-face-attribute 'js2-error nil
                                   :foreground "tomato"))))

(provide 'init-themes-customization-for-weyland)