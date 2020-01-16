;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(use-package company
  :diminish
  :defer t
  :ensure t
  :config
  (progn
    (setq completion-ignore-case t)
    (setq company-dabbrev-downcase nil)
    ;; Show suggestions after entering one character.
    (setq company-minimum-prefix-length 1)
    ;; wrap around to the top of the list again
    (setq company-selection-wrap-around t)
    (setq company-echo-delay 0.01)
    (setq company-idle-delay 0.01)
    (setq company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode))
    ;; (setq company-frontends
    ;;       '(company-pseudo-tooltip-frontend
    ;;         company-echo-metadata-frontend))
    
    (define-key company-active-map [tab] 'company-complete-selection)
    ;; (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)

    ;; company-capf, company-dabbrev and company-files are very useful. So, adjust default backends
    (defvar company-default-backends '(company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                                                    (company-dabbrev-code company-gtags company-etags company-keywords)
                                                    company-oddmuse company-dabbrev))    
    (defvar company-my-backends '(company-capf company-dabbrev company-files company-semantic
                                               (company-dabbrev-code company-gtags company-etags company-keywords)
                                               company-oddmuse company-dabbrev))
    (setq company-backends company-my-backends)))

;; use statistics to better filter completion candidates
(use-package company-statistics
  :init
  (add-hook 'company-mode-hook #'company-statistics-mode)
  (setq company-transformers '(company-sort-by-statistics
                               company-sort-by-backend-importance))
  :after (company)
  :defer t
  :ensure t)


(use-package company-posframe
  :after company
  :config
  (setq company-posframe-show-indicator nil)
  (setq company-posframe-show-metadata nil)
  (company-posframe-mode 1))

(add-hook 'after-init-hook 'global-company-mode)


(provide 'init-company)
;;; init-company.el ends here
