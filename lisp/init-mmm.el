;;; init-mmm.el --- Multiple Major Modes support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ;;----------------------------------------------------------------------------
;; ;; Multiple major modes
;; ;;----------------------------------------------------------------------------
;; ;; (require-package 'mmm-mode)
;; (use-package mmm-mode
;;   :ensure t
;;   :config
;;   (progn
;;     (setq mmm-global-mode 'buffers-with-submode-classes)
;;     (setq mmm-submode-decoration-level 2)))


(add-to-list 'load-path
             "~/.emacs.d/site-lisp/mmm-mode")
(require 'mmm-mode)

(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 1)

(set-face-background 'mmm-default-submode-face (face-attribute 'default :background))

;; (setq mmm-submode-decoration-level 0)
(setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))

(provide 'init-mmm)
;;; init-mmm.el ends here
