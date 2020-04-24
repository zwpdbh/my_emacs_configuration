(use-package aggressive-indent
  :ensure t
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))


;; define additional minor mode to adjust keybindings without conflicts
(defvar my-lisp-power-map (make-keymap))
(define-minor-mode my-lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap my-lisp-power-map)
(define-key my-lisp-power-map [delete] 'paredit-forward-delete)
(define-key my-lisp-power-map [backspace] 'paredit-backward-delete)

;; define a group of common features needed by all lisp programming
(defun zwpdbh/enhance-lisp-power ()
  (interactive)
  (my-lisp-power-mode t)
  (turn-on-eldoc-mode)
  (subword-mode t)
  (paredit-mode t)
  (rainbow-delimiters-mode-enable)
  (aggressive-indent-mode t))

;; define a group of different lisp modes, so we could apply features on on them 
(setq my-lisp-mode-set '(lisp-mode
                         lisp-interaction-mode
                         emacs-lisp-mode
                         sly-mode
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
                                (add-hook (intern (format "%s-hook" each-mode))
                                          #'zwpdbh/enhance-lisp-power))))

(provide 'init-lisp-tool)
