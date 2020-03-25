

;; ===== set font 
;; https://github.com/adobe-fonts/source-code-pro
;; to adjust font dynamically
;; C-xC-+ and C-xC-- to increase or decrease the buffer text size
(set-face-attribute 'default nil :height 120)

(if (member "Source Code Pro" (font-family-list))
    (set-frame-font "Source Code Pro 11"))
;; check current font are using: http://ergoemacs.org/emacs/emacs_list_and_set_font.html

;; ===== install all the icons 
(use-package all-the-icons
  :ensure t 
  :defer t)
;; After the package installed, run ~M-x all-the-icons-install-fonts~
;; For Windows10, after executed the above command, go to the place specified to manually install theme.


;; ===== set mode-line 
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


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


;; ===== adjust keybindings
(require-init 'init-keybinding)


;; ===== set buffer and shell 
(use-package exec-path-from-shell
  :defer 2
  :ensure t
  :config
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

;; ===== use y-or-n 
(fset 'yes-or-no-p 'y-or-n-p)

;; ===== adaptive-wrap
(maybe-require-package 'adaptive-wrap)
(global-visual-line-mode t)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)

;; ===== highlight current line 
(global-hl-line-mode +1)

;; ===== try 
(use-package try
  :commands (try)
  :ensure t)

;; ===== trump-mode
(setq tramp-default-method "ssh")

;; ===== display pretty characters
;; common symbols: http://xahlee.info/comp/unicode_punctuation_symbols.html
;; place to find map between unicode and symbol: https://www.fileformat.info/info/unicode/char/2264/index.htm
(define-globalized-minor-mode my-global-prettify-symbols-mode prettify-symbols-mode
  (lambda ()
    (setq prettify-symbols-alist
          '(
            ("lambda" . 955) ; λ
            ("->" . 8594)    ; →
            ("<-" . 8592)    ; ←
            ("=>" . 8658)    ; ⇒
            ("<=" . 8656)    ; ⇐
            ("map" . 8614)   ; ↦
            ("checkmark" . 10003)   ; ✓
            ))
    (prettify-symbols-mode 1)))

(add-hook 'after-init-hook '(lambda ()
                              (my-global-prettify-symbols-mode 1)))

;; ===== make window/buffer move easier 
(use-package buffer-move
  :ensure t 
  :defer t)

(provide 'init-interface-tweaks)
