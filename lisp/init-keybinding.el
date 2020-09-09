;; note about keybindings in terminal, two scnarios could happen
;; 1) key from outside is not mapped currectly
;; 2) key from outside is not mapped at all!
(unless (display-graphic-p)
  ;; functions will remap keys
  ;; (define-key key-translation-map (kbd "<C-left>") (kbd "<M-left>"))
  ;; (define-key key-translation-map (kbd "<C-right>") (kbd "<M-right>"))
  
  ;; key event from keyboard through MobaXterm should be encoded as below.
  ;; their value could be check by "showkey -a"
  (define-key input-decode-map "^[/" [M-/])
  (define-key input-decode-map "^_" [C-/])
  (define-key input-decode-map "^[[1;5D" [C-left])
  (define-key input-decode-map "^[[1;5C" [C-right])
  (define-key input-decode-map "^[[1;3D" [M-left])
  (define-key input-decode-map "^[[1;3C" [M-right]))

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

(add-hook 'after-init-hook
          '(lambda ()
             (global-set-key (kbd "M-.") 'xref-find-definitions)
             (global-set-key (kbd "M-/") 'xref-find-references)))

;; adjust key-bindings for xref
(defun zw/customize-xref-key-bindings ()
  (interactive)
  (define-key (current-local-map) (kbd "M-.") 'xref-find-definitions)
  (define-key (current-local-map) (kbd "M-/") 'xref-find-references)
  (when (fboundp 'counsel-etags-find-tag-at-point)
    (define-key (current-local-map) [remap xref-find-definitions] 'counsel-etags-find-tag-at-point)
    (define-key (current-local-map) [remap xref-find-references] 'counsel-etags-list-tag)))

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

;; when runing in -nw, use xclip
(unless (display-graphic-p)
  (global-set-key [remap whole-line-or-region-kill-ring-save] 'copy-to-x-clipboard)
  (global-set-key [remap whole-line-or-region-kill-region] 'cut-to-x-clipboard)
  (global-set-key [remap whole-line-or-region-yank] 'paste-from-x-clipboard)
  (when (fboundp 'paste-from-x-clipboard)
    (global-set-key [remap yank] 'paste-from-x-clipboard)))

(provide 'init-keybinding)
