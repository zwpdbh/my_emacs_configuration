(setq coding-system-for-write 'utf-8-unix)
;; ===== set buffer and shell 
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (progn
;;     (when (memq window-system '(mac ns x))
;;       (exec-path-from-shell-initialize))))

;; Show actual color from hex code or color string.
;; Run "M-x rainbow-mode" when needed.
(maybe-require-package 'rainbow-mode)

;; ===== try 
(use-package try
  :commands (try)
  :ensure t)

(use-package expand-region
  :ensure t
  :config
  (progn
    (global-set-key (kbd "M-=") 'er/expand-region)
    (global-set-key (kbd "M--") 'er/contract-region)))

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Instant scratch buffer for current mode
;; https://github.com/ieure/scratch-el
(add-to-list 'load-path
             "~/.emacs.d/site-lisp/scratch-el")
;; uses package "scratch"
(autoload 'scratch "scratch" nil t)
;; M-x scratch, Immediately create a scratch buffer with the same major mode as the current buffer’s.
;; C-u M-x scratch, Prompts for a major mode to create a scratch buffer with.

;; custom modeline to show file name
(setq-default mode-line-buffer-identification
              '(:eval (let ((fullname (buffer-file-name (current-buffer))))
                        (if (not fullname)
                            (buffer-name)
                          (let* ((splited (split-string fullname "/"))
                                 (filename-part (last splited))
                                 (except-last (butlast splited))
                                 (parent-folder (last except-last)))
                            (if parent-folder
                                (let ((project-path (my/project-root)))
                                  (if project-path
                                      (file-relative-name fullname project-path)
                                    (concat (car parent-folder) "/" (car filename-part))))
                              fullname))))))


(provide 'init-convenient)
