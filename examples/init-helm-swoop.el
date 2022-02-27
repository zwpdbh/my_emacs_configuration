;; ref: https://github.com/emacsorphanage/helm-swoop
(when (maybe-require-package 'helm-swoop)
  (setq helm-swoop-use-fuzzy-match t)
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-move-to-line-cycle t)
  (setq helm-swoop-split-with-multiple-windows t)
  
  (setq helm-swoop-pre-input-function
        (lambda () (thing-at-point 'symbol)))

  ;; (global-set-key (kbd "C-c C-s") 'helm-swoop)
  (after-load 'helm-swoop
    (define-key helm-swoop-map (kbd "C-p") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-n") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-p") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-n") 'helm-next-line)))

(provide 'init-helm-swoop)