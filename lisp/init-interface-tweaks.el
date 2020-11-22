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

;; prettify-symbols
(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))


(provide 'init-interface-tweaks)
