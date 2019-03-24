(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
;; Use my init file in org-mode to set other packages
;;(org-babel-load-file (expand-file-name "~/.emacs.d/myinit_for_windows.org"))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; auto-package-update
(use-package auto-package-update
  :ensure t
  :config
  (progn
    ;; pckages will be updated every 100 days
    ;; the old ones will be removed
    (setq auto-package-update-delete-old-versions t
	  auto-package-update-interval 100)
    (auto-package-update-maybe)))


;; set font 
(cond
 ((string-equal system-type "windows-nt")
  (set-default-font "Consolas 11")))


;; set global keybindings
;; use cmd + n and cmd + p to select next and previous lines
(global-set-key (kbd "s-n") (kbd "C-S-n"))
(global-set-key (kbd "s-p") (kbd "C-S-p"))

;; use cmd + / to comment region of code
(global-set-key (kbd "s-/") 'comment-region)
;; use cmd + \ to uncomment region of code
(global-set-key (kbd "s-\\") 'uncomment-region)

;; use c-c c-c to execute a function in scheme
(global-set-key (kbd "C-c C-c") 'eval-last-sexp)

;; use f4 to format whole buffer
(global-set-key (kbd "<f10>") (kbd "C-x h C-M-\\"))
;; use f3 to use org-edit-special
(global-set-key (kbd "<f3>") (kbd "C-c '"))
;; use f5 to revert the buffer
(global-set-key (kbd "<f5>") 'revert-buffer)
;; use f6 to toggle the display of inline image
(global-set-key (kbd "<f6>") (kbd "C-c C-x C-v")) 


;; no need for prompt for confirm when execute code block in org-mode
(setq org-confirm-babel-evaluate nil)

;; make sure environment variables inside Emacs look the same as in the user's shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

;; set line number
(use-package nlinum
  :defer 1
  :ensure t
  :config
  (progn
    (global-nlinum-mode t)
    ;; Preset `nlinum-format' for minimum width.
    (defun my-nlinum-mode-hook ()
      (when nlinum-mode
	(setq-local nlinum-format
		    (concat "%" (number-to-string
				 ;; Guesstimate number of buffer lines.
				 (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
			    "d"))))
    (add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

    ;; [[https://www.emacswiki.org/emacs/LineNumbers][solve bug with emacs daemon mode]]
    (defun initialize-nlinum (&optional frame)
      (require 'nlinum)
      (add-hook 'prog-mode-hook 'nlinum-mode))
    (when (daemonp)
      (add-hook 'window-setup-hook 'initialize-nlinum)
      (defadvice make-frame (around toggle-nlinum-mode compile activate)
	(nlinum-mode -1) ad-do-it (nlinum-mode 1)))))



;; set adaptive-wrap
(use-package adaptive-wrap
  :defer 1
  :ensure t
  :config
  (progn
    ;; (setq-default adaptive-wrap-extra-indent 2)
    (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
    (global-visual-line-mode 1)))

;; change the cursor type and color 
(setq-default cursor-type '(hbar . 2))

;; highlight current line
(global-hl-line-mode +1)

;; use indent-guide
(use-package indent-guide
  :defer 1
  :ensure t
  :config
  (indent-guide-global-mode))

;; which-key 
(use-package which-key
  :defer t
  :ensure t
  :config (which-key-mode))

;; try
(use-package try
  :defer t
  :ensure t)

;; Swipe/Ivy/Counsel
;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))


(use-package swiper
  :ensure try
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))


;; keep parentheses balanced
(use-package paredit
  :defer 1
  :ensure t
  :init
  (progn
    (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    (add-hook 'racket-mode-hook           #'enable-paredit-mode)

    ;; paredit with eldoc
    (require 'eldoc) ; if not already loaded
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round)

    ;; paredit with slime repl
    (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
    ;; To alleviate the annoying habit of grabbing DEL in slime's REPL
    ;; Stop SLIME's REPL from grabbing DEL,
    ;; which is annoying when backspacing over a '('
    (defun override-slime-repl-bindings-with-paredit ()
      (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))
    (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

    ;; paredit with electric return
    (defvar electrify-return-match
      "[\]}\)\"]"
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
    ;; Using local-set-key in a mode-hook is a better idea.
    (global-set-key (kbd "RET") 'electrify-return-if-match)))

;; some complements to paredit
(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))

;; autopair
(use-package autopair
  :defer 1
  :ensure t
  :config
  (progn
    (defvar autopair-modes 
      '(js-mode python-mode scala-mode))
    (defun turn-on-autopair-mode () (autopair-mode 1))

    (dolist (mode autopair-modes) 
      (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode)
      (add-hook (intern (concat (symbol-name mode) "-hook")) (lambda ()
							       (push '(?\( . ?\))
								     (getf autopair-extra-pairs :code)))))

    (add-hook 'typescript-mode-hook 'turn-on-autopair-mode)
    (add-hook 'typescript-mode-hook (lambda ()
				      (push '(?( . ?)) 
					    (getf autopair-extra-pairs :code))))

    ;; Autopair doesn鈥檛 make much sense when paredit-mode is turned on, 
    ;; so it actually defers to paredit-mode when that is installed and enabled. 
    ;; Therefore, disable autopair when paredit is turned on
    (defadvice paredit-mode (around disable-autopairs-around (arg))
      ad-do-it
      (if (null ad-return-value)
	  (autopair-mode 1)
	(autopair-mode 0)))
    (ad-activate 'paredit-mode)))


;; Company for autocomplete
(use-package company
  :defer t
  :ensure t
  :config
  (progn
    (setq completion-ignore-case t)
    (setq company-dabbrev-downcase 0)
    (setq company-idle-delay 0.01)
    (add-hook 'after-init-hook 'global-company-mode)))


;; Ace-window
(use-package ace-window
  :ensure t
  :init
  :config
  (progn
    (setq aw-scope 'frame)
    (global-set-key (kbd "C-x O") 'other-frame)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

;; set Emacs theme 
(use-package leuven-theme
  :ensure t
  :config
  (progn
    (load-theme 'leuven t)
    ;; highlight matched parenthesis
    (set-face-foreground 'show-paren-match "red")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)))


;; Lisp programming
;; eldoc
(use-package eldoc
  :defer 3
  :ensure t
  :init
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
    ;; highlight eldoc arguments in emacslisp
    (defun eldoc-get-arg-index ()
      (save-excursion
	(let ((fn (eldoc-fnsym-in-current-sexp))
	      (i 0))
	  (unless (memq (char-syntax (char-before)) '(32 39)) ; ? , ?'
	    (condition-case err
		(backward-sexp)             ;for safety
	      (error 1)))
	  (condition-case err
	      (while (not (equal fn (eldoc-current-symbol)))
		(setq i (1+ i))
		(backward-sexp))
	    (error 1))
	  (max 0 i))))

    (defun eldoc-highlight-nth-arg (doc n)
      (cond ((null doc) "")
	    ((<= n 0) doc)
	    (t
	     (let ((i 0))
	       (mapconcat
		(lambda (arg)
		  (if (member arg '("&optional" "&rest"))
		      arg
		    (prog2
			(if (= i n)
			    (put-text-property 0 (length arg) 'face 'underline arg))
			arg
		      (setq i (1+ i)))))
		(split-string doc) " ")))))

    (defadvice eldoc-get-fnsym-args-string (around highlight activate)
      ""
      (setq ad-return-value (eldoc-highlight-nth-arg ad-do-it
						     (eldoc-get-arg-index))))))


;; common lisp
;; slime for common-lisp 
(use-package lisp-mode
  :defer 1
  :config
  (progn
    (use-package elisp-slime-nav
      :defer t
      :ensure t
      :commands elisp-slime-nav-mode)
    (use-package macrostep
      :defer t
      :ensure t
      :bind ("C-c e" . macrostep-expand))
    (use-package slime
      :defer t
      :ensure t
      :commands (slime slime-lisp-mode-hook)
      :config
      (progn
	;; make sbcl and slime accessible from command line
	(if (string-equal system-type "windows-nt")
	    (progn
	      ;; Or, just edit path variable of system
	      ;; (add-to-list 'load-path "D:\\Program Files\\Lisp\\sbcl")
	      (add-to-list 'load-path "C:\\clisp-2.49")
	      (add-to-list 'load-path "D:\\Program Files\\slime"))
	  (progn
	    (setq exec-path (append exec-path
				    '("/usr/local/bin")))))
	;; set 'sbcl' as lisp compiler
	;; (setq inferior-lisp-program "sbcl")
	(setq inferior-lisp-program "clisp")
	;; (add-to-list 'slime-contribs 'slime-fancy)
	;; (slime-setup)
	(require 'slime-autoloads)
	(eval-after-load 'slime '(progn
				   (slime-setup '(slime-fancy))))
	;; (use-package ac-slime
	;;   :ensure t
	;;   :config
	;;   (progn
	;;     (add-hook 'slime-mode-hook 'set-up-slime-ac)
	;;     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
	;;     (eval-after-load "auto-complete"
	;;       '(add-to-list 'ac-modes 'slime-repl-mode))))
	(use-package slime-company
	  :defer t
	  :ensure t
	  :config
	  (progn
	    (slime-setup '(slime-fancy slime-company))))))))


;; Racket 
(use-package racket-mode
  :defer t
  :ensure t
  :config
  (progn
    (if (string-equal system-type "windows-nt")
	(setq racket-program "c:/Program Files/Racket/Racket.exe")
      (setq racket-program "/Applications/Racket_v7.0/bin/racket"))
    (add-hook 'racket-mode-hook
	      (lambda ()
		(define-key racket-mode-map (kbd "C-c r") 'racket-run)))
    (setq tab-always-indent 'complete)
    (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
    (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

    ;; setup file ending in ".scheme" to open in racket-mode 
    (add-to-list 'auto-mode-alist '("\\.scheme\\'" . racket-mode))))


;; ensime for scala programming
(use-package ensime
  :defer 1
  :init 
  (if (string-equal system-type "windows-nt")
      (progn
	(setq exec-path (append exec-path '("c:/Program Files (x86)/scala/bin")))
	(setq exec-path (append exec-path '("c:/Program Files (x86)/sbt/bin"))))
    (setq exec-path (append exec-path '("/usr/local/bin"))))
  :ensure t
  :config
  (progn
    ;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
    (add-hook 'scala-mode-hook 'ensime-mode)))


;; Org-mode 
(add-hook 'org-mode-hook
	  (lambda () (local-set-key (kbd "<f9>") #'org-global-cycle)))
;; make code-block could be executed in org-mode
(cond
 ((string-equal system-type "darwin")
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (lisp . t)
       (C . t)))))
 ((string-equal system-type "gnu/linux")
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sh . t)
       (C . t)))))
 ((string-equal system-type "windows-nt")
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (lisp . t)
       (C . t))))))


;; convert buffer text and decorations to HTML 
(use-package htmlize
  :defer 2
  :ensure t)


;; treemacs 
(use-package treemacs
  :defer 2
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
	  treemacs-file-event-delay           5000
	  treemacs-follow-after-init          t
	  treemacs-follow-recenter-distance   0.1
	  treemacs-goto-tag-strategy          'refetch-index
	  treemacs-indentation                2
	  treemacs-indentation-string         " "
	  treemacs-is-never-other-window      nil
	  treemacs-no-png-images              nil
	  treemacs-project-follow-cleanup     nil
	  treemacs-recenter-after-file-follow nil
	  treemacs-recenter-after-tag-follow  nil
	  treemacs-show-hidden-files          t
	  treemacs-silent-filewatch           nil
	  treemacs-silent-refresh             nil
	  treemacs-sorting                    'alphabetic-desc
	  treemacs-tag-follow-cleanup         t
	  treemacs-tag-follow-delay           1.5
	  treemacs-width                      40)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
		 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
	([f8]        . treemacs)
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :defer t
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :defer t
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :defer t
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :defer t
  :after treemacs magit
  :ensure t)


;; Yaml-mode
(use-package yaml-mode
  :defer t
  :ensure t
  :config
  (progn
    (add-hook 'yaml-mode-hook
	      (lambda ()
		(define-key yaml-mode-map "\C-m" 'newline-and-indent)))))



;; Flycheck
(use-package flycheck-yamllint
  :defer t
  :ensure t
  :defer t
  :init
  (progn
    (use-package flycheck
      :ensure t)
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

;; Typescript with tide 
(use-package tide
  :mode "\\.ts\\'"
  :ensure t
  :config
  (progn
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode + 1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      (indent-guide-mode +1)
      (auto-complete-mode +1))

    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)

    ;; formats the buffer before saving
    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)))

;; javascript 
;; js2-mode 
;; - Using js2-refactor
;;      - It is a javascript refactoring libary for emacs
;;      - see full list of keybindings [[https://github.com/magnars/js2-refactor.el][README]]
;;    - Using xref-js2
;;      - It supports for quickly jumping to function definitions or references to JavaScript projects in Emacs
;;      - Keybindings
;;        - M-. jump to definition
;;        - M-? jump to references
;;        - M-, Pop back to where M. was last invoked.
(use-package js2-mode
  :defer t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) ;; equal effect with  :mode "\\.js\\'"
    (add-hook 'js-mode-hook #'js2-imenu-extras-mode)
    
    (use-package js2-refactor
      :defer t
      :ensure t
      :config
      (progn
	(add-hook 'js2-mode-hook #'js2-refactor-mode)
	(js2r-add-keybindings-with-prefix "C-c C-r")
	(define-key js2-mode-map (kbd "C-k") #'js2r-kill)))

    (use-package xref-js2
      :defer t
      :ensure t
      :config
      (progn
	;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so unbind it.
	(define-key js-mode-map (kbd "M-.") nil)
	(add-hook 'js2-mode-hook (lambda ()
				   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))))

;;  setting up term and company-mode for auto-completion
(use-package tern
  :mode "\\.js\\'"
  :ensure t
  :config
  (progn
    (use-package company-tern
      :defer t
      :ensure t
      :config
      (progn
	(add-to-list 'company-backend 'company-tern)
	(add-hook 'js2-mode-hook (lambda ()
				   (tern-mode)
				   (company-mode)))
	;; Disable completion keybindings, as we use xref-js2 instead
	(define-key tern-mode-keymap (kbd "M-.") nil)
	(define-key tern-mode-keymap (kbd "M-,") nil)))))


;; Python development
;; require rop, jedi, flake8, importmagic
;; elpy

(use-package elpy
  :mode "\\.py\\'"
  :ensure t
  :init
  (progn
    (setq elpy-rpc-backed "jedi")
    (elpy-enable))
  :config
  (progn
    (add-hook 'python-mode-hook 'elpy-mode)
    (with-eval-after-load 'elpy
      (add-hook 'elpy-mode-hook 'elpy-use-ipython)))
  :bind
  (("M-*" . pop-tag-mark)))

;; indent-tools 
(use-package indent-tools
  :defer t
  :ensure t
  :init
  (progn
    (add-hook 'python-mode-hook (lambda ()
				  (define-key python-mode-map (kbd "C-c i") 'indent-tools-hydra/body)))))

;; web mode 
(use-package web-mode
  :mode "\\.html\\'"
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (setq web-mode-engines-alist '(("django" . "\\.html\\'")))

    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)

    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-auto-expanding t)
    (setq web-mode-enable-css-colorization t)))


;; JSON-mode 
(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)
