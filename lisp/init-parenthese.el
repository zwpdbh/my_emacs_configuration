;;; -*- lexical-binding: t; -*-

;; electric return in parenthesis
(defvar electrify-return-match
  "[\]}\)\"\']"
  "If this regexp matches the text after the cursor, do an \"electric\"
        return.")
(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
        open and indent an empty line between the cursor and the text.  Move the
        cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))
(defun zw/set-electrify-return ()
  (define-key (current-local-map) (kbd "RET") 'electrify-return-if-match))
(defun zw/unset-electrify-return ()
  (local-unset-key (kbd "RET"))
  (define-key (current-local-map) (kbd "RET") 'newline-and-indent))

(add-hook 'prog-mode-hook 'zw/set-electrify-return)
(add-hook 'conf-mode-hook 'zw/set-electrify-return)

(add-hook 'c-mode-hook 'zw/unset-electrify-return)
(add-hook 'c++-mode-hook 'zw/unset-electrify-return)
(add-hook 'lisp-mode-hook 'zw/unset-electrify-return)
(add-hook 'emacs-lisp-mode-hook 'zw/unset-electrify-return)

(when (maybe-require-package 'paredit)
  ;; Show matching parenthesis
  (require 'paren))

(when (maybe-require-package 'smartparens)
  (require 'smartparens-config)

  ;; add addition pairs for certain mode
  ;; It is not a solution, since in c++, we often have cout <<
  ;; (sp-local-pair 'c-mode "<" ">")
  ;; (sp-local-pair 'c++-mode "<" ">")
  
  (smartparens-global-mode t))

(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;; ===show-paren-mode===
;; show matching lines when parentheses go off-screen
;; ref: https://with-emacs.com/posts/ui-hacks/show-matching-lines-when-parentheses-go-off-screen/
;; It need to be used with lexical-binding

;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)
;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)

(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
   (defun show-paren--off-screen+ (&rest _args)
     "Display matching line for off-screen paren."
     (when (overlayp ov)
       (delete-overlay ov))
     ;; check if it's appropriate to show match info,
     ;; see `blink-paren-post-self-insert-function'
     (when (and (overlay-buffer show-paren--overlay)
                (not (or cursor-in-echo-area
                         executing-kbd-macro
                         noninteractive
                         (minibufferp)
                         this-command))
                (and (not (bobp))
                     (memq (char-syntax (char-before)) '(?\) ?\$)))
                (= 1 (logand 1 (- (point)
                                  (save-excursion
                                    (forward-char -1)
                                    (skip-syntax-backward "/\\")
                                    (point))))))
       ;; rebind `minibuffer-message' called by
       ;; `blink-matching-open' to handle the overlay display
       (cl-letf (((symbol-function #'minibuffer-message)
                  (lambda (msg &rest args)
                    (let ((msg (apply #'format-message msg args)))
                      (setq ov (display-line-overlay+
                                (window-start) msg ))))))
         (blink-matching-open))))))

(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(
                            :inherit default 
                            ;; :inherit highlight 
                            :foreground "Yellow" 
                            :weight 'normal)))
    ol))


(show-paren-mode 1)


(provide 'init-parenthese)
