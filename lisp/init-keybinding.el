;; resize bindings
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;; use cmd + n and cmd + p to select next and previous lines
(global-set-key (kbd "s-n") (kbd "C-S-n"))
(global-set-key (kbd "s-p") (kbd "C-S-p"))
;; use c-z to undo
(global-set-key (kbd "C-z") #'undo)

;; do code comment 
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

;; ===== adjust meta key for Mac OSX
(when *is-a-mac*
  ;; use macbook's command(cmd) key as meta key 
  ;; (setq mac-command-modifier 'meta
  ;;       mac-option-modifier 'none)

  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))

  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore))))


;; my function which evaluats and collects keybindings
(setq zw/keybinding-list nil)
(defun zw/add-keybinding-hooks (hook-list keybindings)
  "it is used to eval and collect keybindings. If in pure terminal-text, keybindings will be evaluated for after-make-frame-functions"
  (dolist (each-keybinding keybindings)
    (eval each-keybinding)
    (add-to-list hook-list each-keybinding)))
;; example:
;; (zw/add-keybinding-hooks 'zw/keybinding-list '((print "1111")
;;                                                (print "2222")
;;                                                (print "3333")))
(add-hook 'after-make-frame-functions
          '(lambda ()
             (unless (display-graphic-p)
               (dolist (each-key-binding zw/keybinding-list)
                 (eval each-key-binding)))))

(add-hook 'after-make-frame-functions
          '(lambda ()
             (unless (display-graphic-p)
               (dolist (each-key-binding zw/keybinding-list)
                 (eval each-key-binding)))))

(provide 'init-keybinding)
