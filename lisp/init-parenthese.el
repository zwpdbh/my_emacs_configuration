;; electric return in parenthesis
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
(defun zw/set-electrify-return ()
  (define-key (current-local-map) (kbd "RET") 'electrify-return-if-match))
(defun zw/unset-electrify-return ()
  (local-unset-key (kbd "RET"))
  (define-key (current-local-map) (kbd "RET") 'newline-and-indent))
(add-hook 'prog-mode-hook 'zw/set-electrify-return)
(add-hook 'c-mode-hook 'zw/unset-electrify-return)
(add-hook 'c++-mode-hook 'zw/unset-electrify-return)

(when (maybe-require-package 'paredit)
  ;; Show matching parenthesis
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (require 'paren))

(when (maybe-require-package 'smartparens)
  (require 'smartparens-config)

  ;; add addition pairs for certain mode
  ;; It is not a solution, since in c++, we often have cout <<
  ;; (sp-local-pair 'c-mode "<" ">")
  ;; (sp-local-pair 'c++-mode "<" ">")
  
  (smartparens-global-mode t))

(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(provide 'init-parenthese)
