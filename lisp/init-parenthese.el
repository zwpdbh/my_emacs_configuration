(use-package paredit
  :diminish
  :defer 1
  :ensure t
  :init
  (progn
    ;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    ;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    ;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    ;; ;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    ;; (add-hook 'sly-mode-hook             #'enable-paredit-mode)
    ;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    ;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    ;; (add-hook 'racket-mode-hook           #'enable-paredit-mode)

    ;; paredit with eldoc
    ;; (require 'eldoc) 
    ;; (eldoc-add-command
    ;;  'paredit-backward-delete
    ;;  'paredit-close-round)

    ;; paredit with electric return
    (defvar electrify-return-match
      "[\]}\)\"]"
      "If this regexp matches the text after the cursor, do an \"electric\"
        return.")
    (defun electrify-return-if-match (arg)
      "If the text after the cursor matches `electrify-return-match' then
        open and indent an empty line between the cursor and the text.  Move the
        cursor to the new line."
      (interactive "P")
      (let ((case-fold-search nil))
        (if (looking-at electrify-return-match)
            (save-excursion (newline-and-indent)))
        (newline arg)
        (indent-according-to-mode)))
    ;; Using local-set-key in a mode-hook is a better idea.
    (global-set-key (kbd "RET") 'electrify-return-if-match)))


;; Show matching arenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)
(require 'paren)
;; (set-face-background 'show-paren-match (face-background 'default))

(provide 'init-parenthese)