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
;; show hex color string's corresponding color
(add-hook 'emacs-lisp-mode-hook 'zw/syntax-color-hex)


(defun zw/customize-theme-for-helm ()
  ;; make helm related selection use underline
  (set-face-attribute 'helm-selection nil
                      :underline t
                      :weight 'bold
                      :extend t)
  (setq helm-buffer-max-length 80)
  (set-face-attribute 'helm-match-item nil
                      :weight 'bold
                      :foreground "black"
                      :background "#FFFF00"
                      :underline nil
                      :extend nil))


(defun zw/customize-theme-for-swiper ()
  ;; make swiper use code default color as foreground
  (set-face-attribute 'isearch nil
                      :foreground (face-foreground 'default t t)
                      :weight 'normal)
  
  (set-face-attribute 'swiper-line-face nil
                      :underline t)
  (set-face-attribute 'swiper-match-face-2 nil
                      :foreground "black")
  ;; make selection highlight-background expand full width of the minibuffer
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)

  (set-face-attribute 'swiper-match-face-2 nil
                      :weight 'bold
                      :background "#FFFF00"
                      :underline nil
                      :extend t))


(defun zw/customize-theme-for-org ()
  (visual-line-mode)
  (setq org-link-file-path-type 'adaptive)
  (setq org-hide-emphasis-markers t)
  
  (set-face-attribute 'org-code nil
                      :foreground zw/blue-for-org-code-in-leuven)
  (set-face-attribute 'org-block nil
                      :foreground (face-foreground 'default t t))
  (setq org-emphasis-alist
        '(("*" (:foreground "chocolate"))
          ("/" italic)
          ("_" underline)
          ("=" (:background "maroon" :foreground "white"))
          ("~" (:foreground "DogerBlue"))
          ("+" (:strike-through t)))))


(defun zw/customize-theme-for-company ()
  (set-face-attribute 'company-tooltip-selection nil
                      :weight 'normal))


(defun zw/customize-theme-for-doom-line ()
  (set-face-attribute 'doom-modeline-project-dir nil
                      :weight 'bold
                      :foreground "#99cc00")
  (set-face-attribute 'doom-modeline-buffer-modified nil
                      :weight 'bold
                      :foreground "#ffcc00"))


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

  (set-face-attribute 'fringe nil
                      :foreground "#4C9ED9"
                      :background (face-attribute 'default :background))
  (set-face-attribute 'show-paren-match nil
                      :weight 'normal
                      :underline nil
                      :foreground (face-background 'default t t)
                      :background zw/light-purple)
  
  (set-face-attribute 'success nil
                      :weight 'bold
                      :foreground "#339933")

  (set-face-attribute 'region nil
                      :background "#cce6ff"
                      :extend t))


(provide 'init-themes-customization)