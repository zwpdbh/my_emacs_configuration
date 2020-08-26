;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; show a dashboard


;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'sanityinc/maybe-suspend-frame)


(add-hook 'after-init-hook
          '(lambda ()
             ;;----------------------------------------------------------------------------
             ;; Suppress GUI features
             ;;----------------------------------------------------------------------------
             (setq use-file-dialog nil)
             (setq use-dialog-box nil)
             (setq inhibit-startup-screen t)

             ;;----------------------------------------------------------------------------
             ;; Window size and features
             ;;----------------------------------------------------------------------------
             (when (fboundp 'tool-bar-mode)
               (tool-bar-mode -1))
             (when (fboundp 'tooltip-mode)
               (tooltip-mode -1))
             ;; (when (fboundp 'scroll-bar-mode)
             ;;   (scroll-bar-mode -1))
             ;; (when (fboundp 'menu-bar-mode)
             ;;   (menu-bar-mode -1))

             ;; I generally prefer to hide the menu bar, but doing this on OS X
             ;; simply makes it update unreliably in GUI frames, so we make an
             ;; exception.
             (if *is-a-mac*
                 (add-hook 'after-make-frame-functions
                           (lambda (frame)
                             (set-frame-parameter frame 'menu-bar-lines
                                                  (if (display-graphic-p frame)
                                                      1 0)))))

             (let ((no-border '(internal-border-width . 0)))
               (add-to-list 'default-frame-alist no-border)
               (add-to-list 'initial-frame-alist no-border))

             (when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
               ;; Command-Option-f to toggle fullscreen mode
               ;; Hint: Customize `ns-use-native-fullscreen'
               (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

             (when *is-a-mac*
               (when (maybe-require-package 'ns-auto-titlebar)
                 (ns-auto-titlebar-mode)))


             (setq frame-title-format
                   '((:eval (if (buffer-file-name)
                                (abbreviate-file-name (buffer-file-name))
                              "%b"))))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
