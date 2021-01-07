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
      (setq zw/helm-ff-dir-color "steelblue")
    (setq zw/helm-ff-dir-color "purple"))
  
  (set-face-attribute 'helm-ff-directory nil
                      :foreground zw/helm-ff-dir-color
                      :background (face-background 'default t t)
                      :weight 'bold)
  (set-face-attribute 'helm-ff-dotted-directory nil
                      :foreground zw/helm-ff-dir-color
                      :background (face-background 'default t t)
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
                      :extend t))

(defun zw/customize-leuven-theme-for-ivy ()
  ;; make selection highlight-background expand full width of the minibuffer
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)

  ;; make ivy current match background lighter
  (set-face-attribute 'ivy-current-match nil
                      :weight 'bold
                      :background "#4C9ED9"))


(defun zw/customize-leuven-theme-for-org ()
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
                      :background "#81BBE4"
                      :weight 'normal))


(defun zw/customize-leuven-theme-for-modeline ()

  (set-face-attribute 'doom-modeline-project-dir nil
                      :weight 'bold
                      :foreground "#99cc00")
  (set-face-attribute 'doom-modeline-buffer-modified nil
                      :weight 'bold
                      :foreground "#ffcc00")
  (set-face-attribute 'doom-modeline-info nil
                      :foreground "#99cc00")


  (set-face-attribute 'mode-line nil
                      :background "steelblue"))



(defun zw/customize-general-leuven-theme ()  
  (when window-system
    (set-background-color "honeydew")
    (set-cursor-color zw/red)
    (setq-default cursor-type '(bar . 3)))  

  ;; disable fringe
  (set-fringe-mode 0)

  ;; (set-face-attribute 'fringe nil
  ;;                     :foreground "#4C9ED9"
  ;;                     :background (face-attribute 'default :background))
  
  (set-face-attribute 'success nil
                      :weight 'bold
                      :foreground "#339933")
  (set-face-attribute 'region nil
                      :background "#cce6ff"
                      :extend t))

(defun zw/customize-leuven-theme-for-symbol-overlay ()
  (add-hook 'symbol-overlay-mode-hook
            '(lambda ()
               (set-face-attribute 'symbol-overlay-default-face nil
                                   :inherit nil
                                   :weight 'normal
                                   :foreground (face-foreground 'default t t)
                                   :background (face-background 'default t t)
                                   ;; :underline '(:color "gray")
                                   :underline t))))


(provide 'init-themes-customization-for-leuven)
