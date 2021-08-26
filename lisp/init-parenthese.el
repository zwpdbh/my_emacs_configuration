;;; -*- lexical-binding: t; -*-
;; ===show-paren-mode===
;; show matching lines when parentheses go off-screen
;; ref: https://with-emacs.com/posts/ui-hacks/show-matching-lines-when-parentheses-go-off-screen/
;; It need to be used with lexical-binding
;; we will call `blink-matching-open` ourselves...
(remove-hook 'post-self-insert-hook
             #'blink-paren-post-self-insert-function)
;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)

;; Very useful to see where the parenthese starts
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

;; Display matched parentheses out of screen
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
                     (or  (memq (char-syntax (char-before)) '(?\) ?\$))
                          (memq (char-syntax (char-before)) '(?\} ?\$))))
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


;; Useful for lisp code
(when (maybe-require-package 'paredit)
  ;; Show matching parenthesis
  (require 'paren))


;; Use smartparens to do auto pair for non-lisp code
(when (maybe-require-package 'smartparens)
  (require 'smartparens-config)

  ;; disable smartparens for certain mode
  ;; (add-to-list 'sp-ignore-modes-list 'org-mode)
  
  ;; disable for specific modes
  (after-load 'org
              (sp-local-pair '(org-mode) "~" "~" :actions nil)
              (sp-local-pair '(org-mode) "*" "*" :actions nil))

  ;; add addition pairs for certain mode
  (sp-local-pair 'elixir-mode "<<" ">>")
  ;; It is not a solution, since in c++, we often have cout <<
  ;; (sp-local-pair 'c-mode "<" ">")
  ;; (sp-local-pair 'c++-mode "<" ">")
  
  (when (featurep 'tuareg)
    ;; Auto pair "'" for OCaml
    (sp-local-pair 'tuareg-mode "'" "'"))

  (smartparens-global-mode t))


;; Make parenthesis less obvious for readability
(maybe-require-package 'paren-face)
(defun zw/customize-themes-for-parenthesis ()
  ;; customize how to show matched parentheses: 'expression or 'parenthesis
  (setq show-paren-style 'parenthesis
        show-paren-delay 0
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren nil
        show-paren-when-point-in-periphery t)
  (after-load 'paren-face
    (set-face-attribute 'show-paren-match nil
                        :weight 'bold
                        :underline nil
                        :foreground "#50fa7b"
                        :background (face-background 'default t t))))

(show-paren-mode 1)


(provide 'init-parenthese)
