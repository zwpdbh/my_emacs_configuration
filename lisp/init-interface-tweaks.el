;; ===== set font 
;; https://github.com/adobe-fonts/source-code-pro
;; to adjust font dynamically
;; C-xC-+ and C-xC-- to increase or decrease the buffer text size
(set-face-attribute 'default nil :height 110)

;; (require 'cl)
;; (defun font-candidate (&rest fonts)
;;   "Return existing font which first match."
;;   (find-if (lambda (f) (find-font (font-spec :name f))) fonts))
;; (set-face-attribute 'default
;;                     nil
;;                     :font (font-candidate
;;                            "Tamzen"
;;                            "Source Code Pro")
;;                     :height 110)

(cond
 ((and *win64* (member "Terminus (TTF) for Windows" (font-family-list)))
  (set-frame-font "Terminus (TTF) for Windows 11"))
 ((member "Terminus (TTF)" (font-family-list))
  (set-frame-font "-PfEd-Terminus (TTF)-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1"))
 ((member "Source Code Pro" (font-family-list))
  (set-frame-font "Source Code Pro 11")))
;; check current font are using: http://ergoemacs.org/emacs/emacs_list_and_set_font.html
;; terminus font: https://files.ax86.net/terminus-ttf/#what

;; ===== aligh commands
;; https://www.emacswiki.org/emacs/AlignCommands#toc5
(add-hook 'align-load-hook (lambda ()
                             (add-to-list 'align-rules-list
                                          '(text-column-whitespace
                                            (regexp  . "\\(^\\|\\S-\\)\\([ \t]+\\)")
                                            (group   . 2)
                                            (modes   . align-text-modes)
                                            (repeat  . t)))))

;; ===== make cursor blink time 
(setq blink-cursor-blinks 10)

;; ===== adjust meta key 
(when *is-a-mac*
  (setq mac-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none))



;; ===== use y-or-n 
(fset 'yes-or-no-p 'y-or-n-p)

;; ===== adaptive-wrap
(maybe-require-package 'adaptive-wrap)
(global-visual-line-mode t)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

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
