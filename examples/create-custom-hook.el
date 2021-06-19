;; ref: Does emacs have a hook for when the theme changes?

;; (defvar after-load-theme-hook nil
;;   "Hook run after a color theme is loaded using `load-theme'.")
;; (defadvice load-theme (after run-after-load-theme-hook activate)
;;   "Run `after-load-theme-hook'."
;;   (run-hooks 'after-load-theme-hook))
;; (add-hook 'after-load-theme-hook
;;           (lambda ()
;;             (set-face-attribute 'show-paren-match nil
;;                                 :weight 'normal
;;                                 ;; :underline nil
;;                                 :underline "#cce6ff"
;;                                 :foreground "#cce6ff"
;;                                 :background (face-background 'default t t))))