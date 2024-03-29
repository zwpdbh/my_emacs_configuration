;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation
(add-to-list 'completion-styles 'initials t)

(when (maybe-require-package 'company)
  (setq-default company-backends '((company-dabbrev-code company-capf) company-dabbrev company-keywords))
  (defun zw/set-company-backends-global ()
    (interactive)
    (setq company-backends '((company-dabbrev-code company-capf) company-dabbrev company-keywords)))
  
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-C-/") 'company-complete)
  (when (maybe-require-package 'company-quickhelp)
    (setq company-quickhelp-delay nil)
    (after-load 'company
      #'company-quickhelp-mode
      (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))))

;; Add a new backend to the head of company-backends recursively:
;; If the head is a list, add new backen into the list at head.
(defun zw/add-to-company-backends (v)
  (let ((backends company-backends))
    (zw/add-to-company-backends-aux backends v)))
(defun zw/add-to-company-backends-aux (local-backends v)
  (let ((backends local-backends))
    (if (not (listp (car backends)))
        (add-to-list (quote backends) v)
      (cons (zw/add-to-company-backends-aux (car backends) v)
            (cdr backends)))))

(defun zw/delete-from-company-backends (v)
  (let ((backends company-backends))
    (zw/delete-from-company-backends-aux backends v)))
(defun zw/delete-from-company-backends-aux (local-backends v)
  (let ((backends local-backends))
    (if (not (listp (car backends)))
        (setq backends (remove v backends))
      (let ((rest (cdr backends)))
        (cons (zw/delete-from-company-backends-aux (car backends) v) rest)))))


(after-load 'company
  (zw/set-company-backends-global)

  ;; company-ctags is much faster out of box. No further optimiation needed
  (add-to-list 'load-path "~/.emacs.d/pre-install/company-ctags")
  (unless (featurep 'company-ctags)
    (require 'company-ctags))
  
  (company-ctags-auto-setup)
  
  (diminish 'company-mode)
  ;; (define-key company-mode-map (kbd "M-/") 'company-complete)
  ;; (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  
  (setq-default company-dabbrev-other-buffers t
                company-dabbrev-code-other-buffers 'all
                company-dabbrev-code-everywhere t
                company-tooltip-align-annotations t
                ;; disable company-icon
                company-format-margin-function nil)
  
  (setq company-require-match nil
        company-echo-delay 0.1
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-minimum-prefix-length 2        
        company-dabbrev-minimum-length 3        
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-show-numbers t
        completion-ignore-case t)
  (setq company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode)))

;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(after-load 'company
  (after-load 'page-break-lines
    (defvar-local sanityinc/page-break-lines-on-p nil)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable)))

(when (maybe-require-package 'company-posframe)
  (when (posframe-workable-p)
    (setq company-posframe-show-metadata nil)
    (setq company-posframe-quickhelp-delay 1)
    (setq company-posframe-show-indicator nil)


    (add-hook 'company-posframe-mode-hook
              #'(lambda ()
                 (define-key company-posframe-active-map [(up)] 'company-posframe-quickhelp-scroll-down)
                 (define-key company-posframe-active-map [(down)] 'company-posframe-quickhelp-scroll-up)))
    (add-hook 'company-mode-hook
              #'(lambda ()
                 (company-posframe-mode 1)))))

(when (maybe-require-package 'company-math)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (setq-local company-backends (add-to-list 'company-backends 'company-math-symbols-latex))
                               (setq-local company-backends (add-to-list 'company-backends 'company-latex-commands))
                               (setq-local company-backends (add-to-list 'company-backends 'company-math-symbols-unicode)))))


;; Customize completion-at-point: adds file path completion to the hook
;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/
;; This offer you file completions when your point is at a path.
;; This is especially nice with selectrum which won’t exit file completions after each path level
;; so you can conveniently navigate to the path like you would do with find-file.
(autoload 'ffap-file-at-point "ffap")
(defun complete-path-at-point+ ()
  "Return completion data for UNIX path at point."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table :exclusive 'no))))

(add-hook 'completion-at-point-functions #'complete-path-at-point+ 'append)


(provide 'init-company)
;;; init-company.el ends here
