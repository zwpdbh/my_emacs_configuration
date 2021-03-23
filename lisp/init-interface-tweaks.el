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

;; customize cursor-type
(when window-system
  ;; (set-cursor-color "tomato")
  (setq-default cursor-type t))
;; ===== make cursor blink forever
(setq-default blink-cursor-interval 0.618)
(setq blink-cursor-blinks -1)

;; ===== reply y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; ===== adaptive-wrap
(global-visual-line-mode t)
(set-default 'fill-column 120)
(add-hook 'org-mode-hook 'visual-line-mode)

(when (require 'adaptive-wrap)
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))


;; ===== highlight current line 
(global-hl-line-mode +1)

;; ===== trump-mode
(setq tramp-default-method "ssh")

;; prettify-symbols
;; (when (fboundp 'global-prettify-symbols-mode)
;;   (add-hook 'after-init-hook 'global-prettify-symbols-mode))


;; line number mode settings
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(when (maybe-require-package 'goto-line-preview)
  (global-set-key [remap goto-line] 'goto-line-preview)

  (when (fboundp 'display-line-numbers-mode)
    (defun sanityinc/with-display-line-numbers (f &rest args)
      (let ((display-line-numbers t))
        (apply f args)))
    (advice-add 'goto-line-preview :around #'sanityinc/with-display-line-numbers)))


;; disable line number for certain mode
(dolist (each-mode-hook '(org-mode-hook
                          term-mode-hook
                          eshell-mode-hook))
  (add-hook each-mode-hook
            '(lambda ()
               (display-line-numbers-mode 0))))

(provide 'init-interface-tweaks)
