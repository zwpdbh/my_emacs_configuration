;; ===== aligh commands
;; https://www.emacswiki.org/emacs/AlignCommands#toc5
(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(text-column-whitespace
                           (regexp  . "\\(^\\|\\S-\\)\\([ \t]+\\)")
                           (group   . 2)
                           (modes   . align-text-modes)
                           (repeat  . t)))))

;; ===== make cursor blink time 
(setq blink-cursor-blinks 10)

;; ===== reply y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; ===== adaptive-wrap
(global-visual-line-mode t)
(set-default 'fill-column 120)
;; (add-hook 'org-mode-hook 'visual-line-mode)
(when (require 'adaptive-wrap)
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))


;; ===== highlight current line 
(global-hl-line-mode +1)

;; ===== trump-mode
(setq tramp-default-method "ssh")

;; ;; ===== display pretty characters
;; ;; common symbols: http://xahlee.info/comp/unicode_punctuation_symbols.html
;; ;; place to find map between unicode and symbol: https://www.fileformat.info/info/unicode/char/2264/index.htm
;; (define-globalized-minor-mode my-global-prettify-symbols-mode prettify-symbols-mode
;;   (lambda ()
;;     (setq prettify-symbols-alist
;;           '(
;;             ("lambda" . 955) ; λ
;;             ("->" . 8594)    ; →
;;             ("<-" . 8592)    ; ←
;;             ("=>" . 8658)    ; ⇒
;;             ("<=" . 8656)    ; ⇐
;;             ("map" . 8614)   ; ↦
;;             ("checkmark" . 10003)   ; ✓
;;             ))
;;     (prettify-symbols-mode 1)))

;; (add-hook 'prog-mode-hook '(lambda ()
;;                              (my-global-prettify-symbols-mode 1)))

(provide 'init-interface-tweaks)
