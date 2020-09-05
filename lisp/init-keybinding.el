;; resize bindings
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;; use cmd + n and cmd + p to select next and previous lines
(global-set-key (kbd "s-n") (kbd "C-S-n"))
(global-set-key (kbd "s-p") (kbd "C-S-p"))

;; set paredit-backward and paredit-forward
(global-set-key (kbd "<f8>") (kbd "C-M-f"))
(global-set-key (kbd "<f7>") (kbd "C-M-b"))

;; use c-z to undo
(global-set-key (kbd "C-z") #'undo)

(if (display-graphic-p)
    (global-set-key (kbd "C-/") 'comment-or-uncomment-region)
  ;; That is because in terminal "^/" is mapped into "^_", check this from "showkey -a"
  (global-set-key (kbd "C-_") 'comment-or-uncomment-region))


;; use c-c c-c to execute a lisp function
(global-set-key (kbd "C-c C-c") 'eval-last-sexp)

;; keyboard macro
(global-set-key (kbd "<f1>") #'toggle-full-window)
(global-set-key (kbd "<f2>") #'kmacro-start-macro)
(global-set-key (kbd "<f3>") #'kmacro-end-macro)
(global-set-key (kbd "<f4>") 'call-last-kbd-macro)


;; use f10 to format whole buffer
(global-set-key (kbd "<f10>") (progn
                                #'mark-whole-buffer
                                #'indent-region))


;; adjust key-bindings for xref
(defun zw/customize-xref-key-bindings ()
  (interactive)
  (define-key (current-local-map) (kbd "M-.") 'xref-find-definitions)
  (if (display-graphic-p)
      (define-key (current-local-map) (kbd "M-/") 'xref-find-references)
    (define-key (current-local-map) (kbd "C-x .") 'xref-find-references)))

;; ===== adjust meta key for Mac OSX
(when *is-a-mac*
  ;; use macbook's command(cmd) key as meta key 
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none)

  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))

  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore))))


(unless (display-graphic-p)
  ;; note about keybindings in terminal, two scnarios could happen
  ;; 1) key from outside is not mapped currectly
  ;; 2) key from outside is not mapped at all!

  ;; (add-hook 'after-make-frame-functions
  ;;           '(lambda ()
  ;;              (unless (display-graphic-p)
  ;;                (if (equal major-mode 'org-mode)
  ;;                    (progn
  ;;                      (define-key input-decode-map "\e[1;5D" [M-left])
  ;;                      (define-key input-decode-map "\e[1;5C" [M-right]))
  ;;                  (define-key input-decode-map "\e[1;5D" [C-left])
  ;;                  (define-key input-decode-map "\e[1;5C" [C-right])))))

  ;; functions will remap keys
  ;; (define-key key-translation-map (kbd "<C-left>") (kbd "<M-left>"))
  ;; (define-key key-translation-map (kbd "<C-right>") (kbd "<M-right>"))
  (add-hook 'buffer-list-update-hook
            '(lambda ()
               (unless (display-graphic-p)
                 (if (equal major-mode 'org-mode)
                     (progn
                       (define-key input-decode-map "\e[1;5D" [M-left])
                       (define-key input-decode-map "\e[1;5C" [M-right]))
                   (define-key input-decode-map "\e[1;5D" [C-left])
                   (define-key input-decode-map "\e[1;5C" [C-right])))))
  (add-hook 'org-mode-hook
            '(lambda ()
               (unless (display-graphic-p)
                 (define-key input-decode-map "\e[1;5D" [M-left])
                 (define-key input-decode-map "\e[1;5C" [M-right])))))


(provide 'init-keybinding)
