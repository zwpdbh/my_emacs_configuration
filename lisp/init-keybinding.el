;; Use C-h b runs the command helm-descbinds to show all keybindings

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

;; ===== make window/buffer move easier 
(when (maybe-require-package 'buffer-move)
  (add-hook 'after-init-hook '(lambda ()
                                (global-set-key (kbd "C-x C-<up>") 'buf-move-up)
                                (global-set-key (kbd "C-x C-<left>") 'buf-move-left)
                                (global-set-key (kbd "C-x C-<right>") 'buf-move-right)
                                (global-set-key (kbd "C-x C-<down>") 'buf-move-down))))

;; use cmd + n and cmd + p to select next and previous lines
(global-set-key (kbd "s-n") (kbd "C-S-n"))
(global-set-key (kbd "s-p") (kbd "C-S-p"))

;; for quickly access replace string
(global-set-key (kbd "C-c s") 'replace-string)

;; set paredit-backward and paredit-forward
;; (global-set-key (kbd "<f8>") (kbd "C-M-f"))
;; (global-set-key (kbd "<f7>") (kbd "C-M-b"))
(global-set-key (kbd "<f8>") 'paredit-forward)
(global-set-key (kbd "<f7>") 'paredit-backward)

;; use f11 to do align text
(global-set-key (kbd "<f11>") 'align-regexp)

;; use c-z to undo
(global-set-key (kbd "C-z") #'undo)

(if (display-graphic-p)
    (global-set-key (kbd "C-/") 'comment-or-uncomment-region)
  (global-set-key (kbd "C-_") 'comment-or-uncomment-region))

;; use c-c c-c to execute a lisp function
(global-set-key (kbd "C-c C-c") 'eval-last-sexp)


(after-load 'swiper
  (global-set-key (kbd "C-s") 'swiper))
(after-load 'ivy
  (global-set-key (kbd "<f1>") 'ivy-resume))
(global-set-key (kbd "<f2>") #'toggle-full-window)
;; (global-set-key (kbd "<f2>") #'kmacro-start-macro)
;; (global-set-key (kbd "<f3>") #'kmacro-end-macro)
(global-set-key (kbd "<f4>") 'call-last-kbd-macro)


;; use f10 to format whole buffer
(global-set-key (kbd "<f10>") (progn
                                #'mark-whole-buffer
                                #'indent-region))

(add-hook 'after-init-hook
          '(lambda ()
             (setq xref-prompt-for-identifier nil)
             (global-set-key (kbd "M-.") 'xref-find-definitions)
             (global-set-key (kbd "M-/") 'xref-find-references)))

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
  ;; (global-set-key [remap whole-line-or-region-kill-ring-save] 'copy-to-x-clipboard)
  (global-unset-key (kbd "M-w"))
  (global-set-key (kbd "M-w") 'copy-to-x-clipboard)  

  ;; (global-set-key [remap whole-line-or-region-kill-region] 'cut-to-x-clipboard)
  (global-unset-key (kbd "C-w"))
  (global-set-key (kbd "C-w") 'cut-to-x-clipboard)  

  ;; (global-set-key [remap whole-line-or-region-yank] 'paste-from-x-clipboard)
  (when (fboundp 'paste-from-x-clipboard)
    (global-unset-key (kbd "C-y"))
    (global-set-key (kbd "C-y") 'paste-from-x-clipboard)))

;; for ivy minibuffer
(when (fboundp 'ivy-previous-line-and-call)
  (define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-call)
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-call)
  
  (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-line-and-call)
  (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line-and-call)
  
  (define-key ivy-minibuffer-map (kbd "<down>") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "<up>") 'ivy-previous-line))

(provide 'init-keybinding)
