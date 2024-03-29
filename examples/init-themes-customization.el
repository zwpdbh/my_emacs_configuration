(defun zw/customize-pkg-with-fn (pkg fn)
  (if (featurep pkg)
      (funcall fn)
    (after-load pkg
      (funcall fn))))

(defun zw/customize-themes-for-general ()
  ;; weyland default, foreground color: #a9b7ca
  ;; weyland default, background color: #1f2226
  ;; weyland comment, foreground color: #606873

  ;; (set-cursor-color "steelblue")
  ;; (setq-default cursor-type t)
  
  ;; (if (display-graphic-p)
  ;;     (cond ((string-match-p "dracula" (symbol-name custom-enabled-theme))
  ;;            t)
  ;;           (t
  ;;            (set-face-attribute 'default nil
  ;;                                :width 'regular
  ;;                                :foreground "#a9b7ca")
  ;;            (set-face-attribute 'font-lock-comment-face nil
  ;;                                :foreground "#606873")))
  ;;   (progn
  ;;     (set-face-attribute 'default nil                          
  ;;                         :foreground "#c6c6c6")))

  
  
  (cond ;; ((string-match-p "kaolin" (symbol-name custom-enabled-theme))         
   ;;  (set-face-attribute 'font-lock-keyword-face nil
   ;;                      :foreground "#91f368"))
   ((string-match-p "tango-dark" (symbol-name custom-enabled-theme))
    (set-face-attribute 'highlight nil
                        :underline nil
                        :foreground (face-foreground 'default t t)
                        :background "Black"))
   ;; (t
   ;;  (set-face-attribute 'font-lock-keyword-face nil
   ;;                      :foreground "#50fa7b"))
   ))

(defun zw/customize-themes-for-dashboard ()
  (set-face-attribute 'dashboard-items-face nil
                      :underline nil
                      :weight 'normal))

(defun zw/customize-themes-for-lsp-ui ()
  ;; customize lsp-ui-peek appearance
  (set-face-attribute 'lsp-ui-peek-selection nil
                      :foreground (face-foreground 'default t t)
                      :background (face-background 'default nil t)
                      :underline nil
                      :weight 'bold)

  (set-face-attribute 'lsp-ui-peek-highlight nil
                      :foreground "Yellow"
                      :background (face-background 'default t t)
                      :underline nil
                      :box nil
                      :weight 'normal)
  (set-face-attribute 'lsp-ui-peek-peek nil
                      :foreground (face-foreground 'default t t)
                      :background (face-background 'default t t)))


(defun zw/customize-themes-for-company ()
  ;; (set-face-attribute 'company-tooltip-common-selection nil
  ;;                     :inherit nil
  ;;                     :foreground "#a9b7ca"
  ;;                     :weight 'bold)

  (set-face-attribute 'company-tooltip-selection nil
                      :inherit nil
                      ;; :foreground "#a9b7ca"
                      :background "#3b3559")
  
  ;; (set-face-attribute 'company-tooltip-annotation-selection nil
  ;;                     :inherit nil
  ;;                     :foreground "#59b9b4")
  (set-face-attribute 'company-tooltip nil
                      :weight 'normal))


(defun zw/customize-themes-for-helm ()
  ;; make helm related selection use underline
  (set-face-attribute 'helm-selection nil
                      :underline nil
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
                      :weight 'bold)
  (set-face-attribute 'helm-selection nil
                      :inherit nil
                      :underline nil
                      :foreground "yellow"
                      :background "#3b3559"))

(defun zw/customize-themes-for-smartparens ()
  (set-face-attribute 'sp-pair-overlay-face nil
                      :background (face-background 'default t t)
                      :inherit nil)
  (set-face-attribute 'show-paren-mismatch nil      
                      :foreground "WhiteSmoke"
                      :background (face-background 'default t t)))

(defun zw/customize-themes-for-selectrum ()
  (set-face-attribute 'selectrum-current-candidate nil
                      :inherit nil
                      :underline nil
                      :weight 'normal
                      :foreground (face-foreground 'default t t)
                      :background "#3b3559"))

(defun zw/customize-themes-for-flycheck ()
  (set-face-attribute 'flycheck-error nil
                      :underline '(:color "firebrick" :style wave))
  (set-face-attribute 'flycheck-warning nil
                      :underline '(:color "yellow" :style wave)))

(defun zw/customize-themes-for-consult ()
  (cond  ;; ((string-match-p "tango-dark" (symbol-name custom-enabled-theme))
   ;;  (set-face-attribute 'consult-preview-line nil
   ;;                      :inherit nil
   ;;                      :underline nil
   ;;                      :weight 'normal
   ;;                      :foreground "Yellow"
   ;;                      :background "Black"))
   (t
    (set-face-attribute 'consult-preview-line nil
                        :inherit nil
                        :underline nil
                        :weight 'normal
                        :foreground "Yellow"
                        :background "Black")))
  
  ;; (set-face-attribute 'consult-file nil
  ;;                     :inherit nil
  ;;                     :foreground "#a9b7ca")
  )


(defun zw/customize-themes-for-marginalia ()
  (set-face-attribute 'marginalia-documentation nil
                      :underline nil))

(defun zw/customize-themes-for-orderless ()
  (set-face-attribute 'orderless-match-face-0 nil
                      :weight 'normal)
  (set-face-attribute 'orderless-match-face-1 nil
                      :weight 'normal)
  (set-face-attribute 'orderless-match-face-2 nil
                      :weight 'normal)
  (set-face-attribute 'orderless-match-face-3 nil
                      :weight 'normal))



(defun zw/customize-themes-for-web-mode ()
  (set-face-attribute 'web-mode-current-element-highlight-face nil
                      :weight 'normal
                      :foreground "#f5f5f5"
                      :background "#3b3559"))

(defun zw/customize-themes-for-ivy ()
  ;; make selection highlight-background expand full width of the minibuffer
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)

  ;; ;; ;; make ivy current match background lighter
  ;; (set-face-attribute 'ivy-current-match nil
  ;;                     :weight 'normal
  ;;                     :foreground (face-foreground 'default t t)
  ;;                     :background "#3b3559")
  
  ;; ;; Face for ‘ivy’ minibuffer matches covered by inputs
  ;; (set-face-attribute 'ivy-minibuffer-match-face-1 nil
  ;;                     :weight 'normal
  ;;                     :underline nil
  ;;                     :foreground "Yellow"
  ;;                     :background (face-background 'default t t))
  
  ;; ;; Face for ‘ivy’ minibuffer matches inputs 
  ;; (set-face-attribute 'ivy-minibuffer-match-face-2 nil
  ;;                     :weight 'normal
  ;;                     :underline nil
  ;;                     :foreground "Yellow"
  ;;                     :background (face-background 'default t t))
  
  ;; ;; Face for "ivy" minibuffer matches other segement input
  ;; (set-face-attribute 'ivy-minibuffer-match-face-3 nil
  ;;                     :weight 'normal
  ;;                     :underline nil
  ;;                     :foreground "Yellow"
  ;;                     :background (face-background 'default t t))
  )

(defun zw/customize-themes-for-swiper ()
  (set-face-attribute 'swiper-line-face nil
                      :weight 'normal
                      :underline nil
                      :foreground (face-foreground 'default t t)
                      :background "#3b3559"))

(defun zw/customize-themes-for-org ()
  ;; Make org-mode markup symbols invisible
  (setq org-hide-emphasis-markers t)
  
  (setq org-emphasis-alist
        '(("*" (:foreground "gold"))
          ("/" italic)
          ("_" underline)
          ("=" (:background "maroon" :foreground "white"))
          ("~" (:foreground "#86dc2f"))
          ("+" (:strike-through t))))
  
  (set-face-attribute 'org-headline-done nil
                      :weight 'bold
                      :foreground "#606873"
                      :strike-through nil))

(defun zw/customize-themes-for-symbol-overlay ()
  (set-face-attribute 'symbol-overlay-default-face nil
                      :underline t
                      :inherit nil))

(defun zw/customize-themes-for-js2 ()
  (set-face-attribute 'js2-error nil
                      :foreground "tomato"))

(defun zw/customize-themes-for-indent-guide ()
  (set-face-attribute 'indent-guide-face nil
                      :foreground "DimGray"
                      :background (face-background 'default)))

(defun zw/customize-themes-for-eshell ()
  (set-face-attribute 'eshell-prompt nil
                      :weight 'normal
                      :foreground "#86dc2f"))

(defun zw/customize-themes-for-magit ()
  (set-face-attribute 'diff-refine-removed nil
                      :inherit nil
                      :weight 'bold
                      :foreground (face-foreground 'default t t)
                      :background (face-background 'default t t))
  (set-face-attribute 'diff-refine-added nil
                      :inherit nil
                      :weight 'bold
                      :foreground (face-foreground 'default t t)
                      :background (face-background 'default t t)))


(provide 'init-themes-customization)
