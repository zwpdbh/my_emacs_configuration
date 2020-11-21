(defun zw/customize-weyland-theme-for-helm ()
  ;; make helm related selection use underline
  (set-face-attribute 'helm-selection nil
                      :underline t
                      :foreground (face-foreground 'default t t)
                      :weight 'bold
                      :extend t)
  (setq helm-buffer-max-length 36))

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
  
  (add-hook 'after-init-hook
            '(lambda ()
               ;; disable fringe 
               (set-fringe-mode 0))))

(defun zw/customize-weyland-theme-for-swiper ()
  ;; make selection highlight-background expand full width of the minibuffer
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line))

(provide 'init-themes-customization-for-weyland)