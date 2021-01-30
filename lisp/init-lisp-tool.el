(use-package aggressive-indent
  :ensure t
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))


;; define additional minor mode to adjust keybindings without conflicts
(defvar zw/lisp-power-map (make-keymap))
(define-minor-mode zw/lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap zw/lisp-power-map)
(define-key zw/lisp-power-map [delete] 'paredit-forward-delete)
(define-key zw/lisp-power-map [backspace] 'paredit-backward-delete)

(defun zw/set-company-backends-for-lisp ()
  (zw/set-company-backends-global))

;; define a group of common features needed by all lisp programming
(defun zw/enhance-lisp-power ()
  (interactive)
  ;; (zw/set-company-backends-for-lisp)
  (zw/lisp-power-mode t)
  (turn-on-eldoc-mode)
  (subword-mode t)
  (paredit-mode t)
  (rainbow-delimiters-mode-enable)
  (aggressive-indent-mode t)
  (zw/unset-electrify-return))

;; define a group of different lisp modes, so we could apply features on on them 
(setq my-lisp-mode-set '(lisp-mode
                         lisp-interaction-mode
                         emacs-lisp-mode
                         sly-mode
                         slime-repl-mode
                         ielm-mode
                         eval-expression-minibuffer-setup
                         common-lisp-mode
                         racket-mode
                         racket-repl-mode
                         scheme-mode
                         clojure-mode
                         cider-repl-mode
                         geiser-repl-mode))

(add-hook 'after-init-hook '(lambda ()
                              (dolist (each-mode my-lisp-mode-set)
                                (add-to-list 'sp-ignore-modes-list each-mode)
                                (add-hook (intern (format "%s-hook" each-mode))
                                          #'zw/enhance-lisp-power))))

(provide 'init-lisp-tool)
