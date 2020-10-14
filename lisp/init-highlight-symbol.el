;;; Code:

(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook java-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (after-load 'symbol-overlay
    (diminish 'symbol-overlay-mode)
    (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))

;; TODO make those features work
;; (defun highlight-symbol-at-point-all-windows ()
;;   "Toggle highlighting of the symbol at point in all windows."
;;   (interactive)
;;   (let ((symbol (symbol-overlay-get-symbol)))
;;     (unless symbol (error "No symbol at point"))
;;     (save-selected-window                           ; new
;;       (cl-dolist (x (window-list))                  ; new
;;         (select-window x)                           ; new
;;         (if (highlight-symbol-symbol-highlighted-p symbol)
;;             (highlight-symbol-remove-symbol symbol)
;;           (highlight-symbol-add-symbol symbol))))))

;; (define-transient-command symbol-overlay-transient ()
;;   "Symbol Overlay transient"
;;   ["Symbol Overlay"
;;    ["Overlays"
;;     ("." "Add/Remove at point" symbol-overlay-put)
;;     ("k" "Remove All" symbol-overlay-remove-all)
;;     ]
;;    ["Move to Symbol"
;;     ("n" "Next" symbol-overlay-switch-forward)
;;     ("p" "Previous" symbol-overlay-switch-backward)
;;     ]
;;    ["Other"
;;     ("m" "Hightlight symbol-at-point" symbol-overlay-mode)]])
;; (global-set-key (kbd "M-'") 'symbol-overlay-transient)

(provide 'init-highlight-symbol)
;;; init-highlight-symbol.el ends here