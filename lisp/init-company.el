;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation
;; (use-package company
;;   :diminish
;;   :ensure t
;;   :defer t)

;; (use-package company-fuzzy
;;   :diminish
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook '(lambda ()
;;                                (company-fuzzy-mode 1))))

;; (use-package company-quickhelp
;;   :diminish
;;   :ensure t
;;   :defer t)

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-C-/") 'company-complete)
  (when (maybe-require-package 'company-quickhelp)
    (setq company-quickhelp-delay nil)
    (after-load 'company
      #'company-quickhelp-mode
      (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)))
  (when (maybe-require-package 'company-statistics)
    (company-statistics-mode)))

(after-load 'company
  (setq company-backends (delete 'company-ropemacs company-backends))
  (dolist (backend '(company-eclim company-semantic company-capf company-bbdb company-dabbrev-code))
    (setq company-backends (delq backend company-backends)))
  (setq company-backends (cons '(company-capf company-bbdb company-dabbrev-code) company-backends)))

;; company-ctags is much faster out of box. No further optimiation needed
(unless (featurep 'company-ctags) (local-require 'company-ctags))
(company-ctags-auto-setup)

(diminish 'company-mode)
;; (define-key company-mode-map (kbd "M-/") 'company-complete)
;; (define-key company-active-map (kbd "M-/") 'company-other-backend)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(setq-default company-dabbrev-other-buffers 'all
              company-tooltip-align-annotations t)
(setq completion-ignore-case t)
(setq company-show-numbers t)
(setq company-dabbrev-downcase nil)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(setq company-echo-delay 0)
(setq company-idle-delay 0)
(setq company-require-match nil)
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



(provide 'init-company)
;;; init-company.el ends here
