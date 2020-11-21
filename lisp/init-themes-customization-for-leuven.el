(defun zw/customize-leuven-theme-for-helm ()
  ;; make helm related selection use underline
  (set-face-attribute 'helm-selection nil
                      :underline t
                      :weight 'bold
                      :extend t)
  (setq helm-buffer-max-length 36)
  (set-face-attribute 'helm-match-item nil
                      :weight 'normal
                      :foreground (face-foreground 'default t t)
                      :background "#FFFF00"
                      :underline nil
                      :extend nil)
  
  (if (display-graphic-p)
      (setq zw/helm-ff-dir-color "#ccccff")
    (setq zw/helm-ff-dir-color "purple"))
  
  (set-face-attribute 'helm-ff-directory nil
                      :foreground zw/helm-ff-dir-color
                      :weight 'bold)
  (set-face-attribute 'helm-ff-dotted-directory nil
                      :foreground zw/helm-ff-dir-color
                      :weight 'bold))


(defun zw/customize-leuven-theme-for-swiper ()
  ;; make swiper use code default color as foreground
  (set-face-attribute 'isearch nil
                      :foreground (face-foreground 'default t t)
                      :weight 'normal)
  
  (set-face-attribute 'swiper-line-face nil
                      :underline t)
  (set-face-attribute 'swiper-match-face-2 nil
                      :foreground "black")
  (set-face-attribute 'swiper-match-face-2 nil
                      :weight 'bold
                      :background "#FFFF00"
                      :underline nil
                      :extend t)
  ;; make selection highlight-background expand full width of the minibuffer
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)

  ;; make ivy current match background lighter
  (set-face-attribute 'ivy-current-match nil
                      :weight 'bold
                      :background "#4C9ED9"))




(defun zw/customize-leuven-theme-for-org ()
  (visual-line-mode)
  (setq org-link-file-path-type 'adaptive)
  (setq org-hide-emphasis-markers nil)
  
  (set-face-attribute 'org-code nil
                      :foreground zw/blue-for-org-code-in-leuven)
  (set-face-attribute 'org-block nil
                      :foreground (face-foreground 'default t t))
  (setq org-emphasis-alist
        '(("*" (:foreground "chocolate"))
          ("/" italic)
          ("_" underline)
          ("=" (:background "maroon" :foreground "white"))
          ("~" (:foreground "#0595bd"))
          ("+" (:Strike-through t)))))


(defun zw/customize-leuven-theme-for-company ()
  (set-face-attribute 'company-tooltip-selection nil
                      :background "#4C9ED9"
                      :weight 'normal))


(defun zw/customize-leuven-theme-for-doom-modeline ()
  (set-face-attribute 'doom-modeline-project-dir nil
                      :weight 'bold
                      :foreground "#99cc00")
  (set-face-attribute 'doom-modeline-buffer-modified nil
                      :weight 'bold
                      :foreground "#ffcc00")
  (set-face-attribute 'doom-modeline-info nil
                      :foreground "#99cc00")


  (setq-default mode-line-format
                (list
                 ;; day and time
                 '(:eval (propertize (format-time-string " %b %d %H:%M ")
                                     'face 'font-lock-builtin-face))


                 '(:eval (propertize (substring vc-mode 5)
                                     'face 'font-lock-comment-face))

                 ;; the buffer name; the file name as a tool tip
                 '(:eval (propertize " %b "
                                     'face
                                     (let ((face (buffer-modified-p)))
                                       (if face 'font-lock-warning-face
                                         'font-lock-type-face))
                                     'help-echo (buffer-file-name)))

                 ;; line and column
                 " (" ;; '%02' to set to 2 chars at least; prevents flickering
                 (propertize "%02l" 'face 'font-lock-keyword-face) ","
                 (propertize "%02c" 'face 'font-lock-keyword-face)
                 ") "

                 ;; relative position, size of file
                 " ["
                 (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
                 "/"
                 (propertize "%I" 'face 'font-lock-constant-face) ;; size
                 "] "

                 ;; spaces to align right
                 '(:eval (propertize
                          " " 'display
                          `((space :align-to (- (+ right right-fringe right-margin)
                                                ,(+ (string-width org-mode-line-string) (+ 3 (string-width mode-name)))
                                                )))))

                 (propertize org-mode-line-string 'face '(:foreground "#5DD8FF"))

                 ;; the current major mode
                 (propertize " %m " 'face 'font-lock-string-face)
                 ;;minor-mode-alist
                 ))

  (set-face-attribute 'mode-line nil
                      :width 1
                      :background "steelblue")

  )



(defun zw/customize-general-leuven-theme ()  
  (when window-system
    (set-cursor-color zw/red)
    (setq-default cursor-type '(bar . 2)))
  
  (setq show-paren-style 'expression)
  (set-face-attribute 'show-paren-match nil
                      :weight 'normal
                      :underline nil
                      :foreground (face-background 'default t t)
                      :background "honeydew")

  ;; disable fringe
  (add-hook 'after-init-hook
            '(lambda ()
               ;; disable fringe 
               (set-fringe-mode 0)))
  ;; (set-face-attribute 'fringe nil
  ;;                     :foreground "#4C9ED9"
  ;;                     :background (face-attribute 'default :background))
  
  (set-face-attribute 'success nil
                      :weight 'bold
                      :foreground "#339933")
  (set-face-attribute 'region nil
                      :background "#cce6ff"
                      :extend t))


(provide 'init-themes-customization-for-leuven)
