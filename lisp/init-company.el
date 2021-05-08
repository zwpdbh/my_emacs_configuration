;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation
(add-to-list 'completion-styles 'initials t)

(when (maybe-require-package 'company)
  (defun zw/set-company-backends-global ()
    (interactive)
    ;; (setq company-backends '((company-dabbrev-code company-capf) company-keywords company-files company-dabbrev))
    (setq company-backends '((company-capf company-dabbrev-code) company-keywords company-files company-dabbrev)))
  
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-C-/") 'company-complete)
  (when (maybe-require-package 'company-quickhelp)
    (setq company-quickhelp-delay nil)
    (after-load 'company
      #'company-quickhelp-mode
      (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))))

;; Add a new backend to the head of company-backends recursively:
;; If the head is a list, add new backen into the list at head.
(defun zw/add-to-company-backends (backends v)
  (if (not (listp (car backends)))
      (add-to-list (quote backends) v)
    (cons (zw/add-to-company-backends (car backends) v)
          (cdr backends))))


(after-load 'company
  (zw/set-company-backends-global)

  ;; company-ctags is much faster out of box. No further optimiation needed
  (unless (featurep 'company-ctags) (local-require 'company-ctags))
  (company-ctags-auto-setup)
  
  (diminish 'company-mode)
  ;; (define-key company-mode-map (kbd "M-/") 'company-complete)
  ;; (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  
  (setq-default company-dabbrev-other-buffers 'all
                company-dabbrev-code-other-buffers 'all
                company-dabbrev-code-everywhere t
                company-tooltip-align-annotations t)
  
  (setq company-require-match nil
        company-echo-delay 0
        company-idle-delay 0
        company-selection-wrap-around t
        company-minimum-prefix-length 2
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-show-numbers t
        completion-ignore-case t)
  (setq company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)))

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
              '(lambda ()
                 (define-key company-posframe-active-map [(up)] 'company-posframe-quickhelp-scroll-down)
                 (define-key company-posframe-active-map [(down)] 'company-posframe-quickhelp-scroll-up)))
    (add-hook 'company-mode-hook
              '(lambda ()
                 (company-posframe-mode 1)))))

(when (maybe-require-package 'company-math)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (setq-local company-backends (add-to-list 'company-backends 'company-math-symbols-latex))
                               (setq-local company-backends (add-to-list 'company-backends 'company-latex-commands))
                               (setq-local company-backends (add-to-list 'company-backends 'company-math-symbols-unicode))))
  ;; (add-hook 'org-mode-hook (lambda ()
  ;;                            (setq-local company-backends (add-to-list 'company-backends 'company-math-symbols-unicode))
  ;;                            (setq-local company-backends (add-to-list 'company-backends 'company-latex-commands))))
  )


(provide 'init-company)
;;; init-company.el ends here
