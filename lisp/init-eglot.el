(when (maybe-require-package 'eglot)

  (defun zw/eglot-key-bindings-for (some-mode)
    (let ((that-mode-map (concat some-mode "-map")))
      (define-key that-mode-map (kbd "M-.") 'lsp-ui-peek-find-definitions)
      (if (display-graphic-p)
          (define-key that-mode-map (kbd "M-/") 'lsp-ui-peek-find-references)
        (define-key that-mode-map (kbd "C-x .") 'lsp-ui-peek-find-references))))
  
  (dolist (each-hook (list
                      'js-mode-hook
                      'python-mode-hook
                      'sh-mode-hook
                      'c-mode-common-hook
                      'c-mode-hook
                      'c++-mode-hook))
    (add-hook each-hook 'eglot-ensure)))

(provide 'init-eglot)