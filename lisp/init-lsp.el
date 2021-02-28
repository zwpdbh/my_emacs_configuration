;; lsp performance tuning, see
;; https://emacs-lsp.github.io/lsp-mode/page/performance/

(when (maybe-require-package 'lsp-mode)  
  (when (symbol-function 'helm)
    (maybe-require-package 'helm-lsp))
  (when (symbol-function 'treemacs)
    (maybe-require-package 'lsp-treemacs))
  (when (symbol-function 'company-mode)
    (maybe-require-package 'company-lsp))
  (maybe-require-package 'lsp-ui))

(after-load 'lsp-mode
  (setq lsp-idle-delay 0.500)
  (setq lsp-message-project-root-warning t)
  ;; change nil to 't to enable logging of packets between emacs and the LS
  ;; this was invaluable for debugging communication with the MS Python Language Server
  ;; and comparing this with what vs.code is doing
  
  (setq lsp-print-io nil)
  (lsp-ui-mode t))

(after-load 'company-lsp
  (setq company-lsp-cache-candidates nil)
  (setq company-lsp-async t)
  (setq company-lsp-enable-recompletion t))

(after-load 'lsp-ui
            ;; Establishing keybindings for lsp-ui-mode
            ;; Variable lsp-ui-mode-map is available only after lsp-ui.el or lsp-ui.elc is loaded.
            (setq lsp-ui-peek-force-fontify t)
            (setq lsp-ui-imenu-enable t)
            (setq lsp-ui-sideline-ignore-duplicate t)
            (setq lsp-ui-sideline-enable nil)
            (setq lsp-ui-doc-enable nil)
            (setq lsp-ui-peek-fontify 'always)

            (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
            (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

            ;; customize lsp-ui-peek appearance
            (add-hook 'lsp-ui-mode-hook
                      '(lambda ()
                        (set-face-attribute 'lsp-ui-peek-selection nil
                         :foreground (face-foreground 'hl-line t t)
                         :background (face-background 'hl-line nil t)
                         :underline nil
                         :weight 'bold)

                        (set-face-attribute 'lsp-ui-peek-highlight nil
                         :foreground "Yellow"
                         :background (face-background 'default t t)
                         :underline nil
                         :box nil
                         :weight 'normal)
                        (set-face-attribute 'lsp-ui-peek-peek nil
                         :foreground (face-foreground 'default t t)
                         :background (face-background 'default t t)))))


(defun zw/lsp-ui-key-bindings ()
  (interactive)
  (when (featurep 'lsp-ui)
    (define-key (current-local-map) (kbd "M-.") 'lsp-ui-peek-find-definitions)
    (define-key (current-local-map) (kbd "M-/") 'lsp-ui-peek-find-references)))


;; (dolist (each-mode zw/lsp-clients-set)
;;   (let ((each-mode-hook (intern (format "%s-hook" each-mode))))
;;     (add-hook each-mode-hook
;;               #'(lambda ()
;;                   (lsp)
;;                   (zw/customize-lsp-ui-key-bindings)))))


(provide 'init-lsp)
