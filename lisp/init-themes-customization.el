(defun zw/customize-pkg-with-fn (pkg fn)
  (if (featurep pkg)
      (funcall fn)
    (after-load pkg
      (funcall fn))))

;; How to check the face you want to set?
;; Use helm-colors (C-c h c) to filter out different color and view different faces!
(defun zw/customize-themes-for-general ()
  (set-face-attribute 'line-number-current-line nil
                      :weight 'normal))

(defun zw/customize-themes-for-particular-one ()
  (let* ((current-theme (symbol-name custom-enabled-theme))
         (config-file (concat "~/.emacs.d/lisp/my-themes/" "for-" current-theme ".el")))
    (when (file-exists-p config-file)
      (load config-file))))

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
  (set-face-attribute 'company-tooltip nil
                      :weight 'normal)
  (set-face-attribute 'company-tooltip-common nil
                      :weight 'normal
                      :underline nil)
  (set-face-attribute 'company-tooltip-common-selection nil
                      :weight 'normal
                      :underline nil))


(defun zw/customize-themes-for-helm ()
  (set-face-attribute 'helm-ff-directory nil
                      :weight 'normal)
  (set-face-attribute 'helm-ff-dotted-directory nil
                      :weight 'normal)
  (set-face-attribute 'helm-selection nil
                      :underline nil
                      :foreground (face-foreground 'default t t)
                      :weight 'normal
                      :extend t))

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
                      :foreground (face-foreground 'default t t))
  (set-face-attribute 'selectrum-current-candidate nil
                      :background (face-attribute 'helm-selection :background)))

(defun zw/customize-themes-for-flycheck ()
  (set-face-attribute 'flycheck-error nil
                      :underline '(:color "firebrick" :style wave))
  (set-face-attribute 'flycheck-warning nil
                      :underline '(:color "yellow" :style wave)))

(defun zw/customize-themes-for-orderless ()
  (set-face-attribute 'orderless-match-face-0 nil
                      :weight 'normal)
  (set-face-attribute 'orderless-match-face-1 nil
                      :weight 'normal)
  (set-face-attribute 'orderless-match-face-2 nil
                      :weight 'normal)
  (set-face-attribute 'orderless-match-face-3 nil
                      :weight 'normal))

(defun zw/customize-themes-for-ivy ()
  ;; make selection highlight-background expand full width of the minibuffer
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)
  
  (set-face-attribute 'ivy-current-match nil
                      :weight 'normal
                      :background (face-attribute 'helm-selection :background))

  ;; ;; Face for ‘ivy’ minibuffer matches covered by inputs
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil
                      :weight 'normal)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil
                      :weight 'normal)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil
                      :weight 'normal)
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil
                      :weight 'normal))

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
