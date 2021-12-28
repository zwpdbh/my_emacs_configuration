;; ref: https://www.emacswiki.org/emacs/AutoIndentation
(when (eval-when-compile (version< "24.4" emacs-version))
  (add-hook 'after-init-hook 'electric-indent-mode))

;; indent guide
(use-package highlight-indent-guides
  :ensure t
  :config 
  (progn
    (setq highlight-indent-guides-delay 0.1)
    ;; (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
    ;; (add-hook 'plantuml-mode-hook 'highlight-indent-guides-mode)
    (add-hook 'json-mode-hook 'highlight-indent-guides-mode)
    (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)))

(use-package indent-guide  
  :ensure t
  :config
  (progn
    (add-hook 'web-mode-hook 'indent-guide-mode)
    (add-hook 'sgml-mode-hook 'indent-guide-mode)
    (add-hook 'org-mode-hook 'indent-guide-mode)))

;; https://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode/10439239
(setq tab-always-indent 'complete)
(setq-default tab-width 2)
(setq tab-width 2)
(setq-default standard-indent tab-width)
(setq-default indent-tabs-mode nil)

(defun zw/adjust-local-tab-width (offset)
  (setq-local tab-width offset))

;; Two callable functions for enabling/disabling tabs in Emacs
(defun zw/disable-tabs ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun zw/enable-tabs  ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

;; All the mode in which indentation could insert tabs
;; Hooks to Enable Tabs
(add-hook 'plantuml-mode-hook
          '(lambda ()
             ;; plantuml seems always use tabs to do indent format
             (zw/enable-tabs)
             (setq plantuml-indent-level tab-width)))
(add-hook 'text-mode-hook 'zw/enable-tabs)


;; All the mode in which indentation could not insert tabs
;; Hooks to Disable Tabs, since tab usually cause inconsistent visual appearence
(add-hook 'prog-mode-hook 'zw/disable-tabs)
(add-hook 'org-mode-hook 'zw/disable-tabs)
(add-hook 'json-mode-hook 'zw/disable-tabs)
(add-hook 'lisp-mode-hook 'zw/disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'zw/disable-tabs)
(add-hook 'yaml-mode-hook 'zw/disable-tabs)
;; Adjust indent offset for specific mode
(add-hook 'python-mode-hook
          '(lambda ()
             (zw/adjust-local-tab-width 4)
             (setq python-indent-offset tab-width)))


;; From https://www.emacswiki.org/emacs/AutoIndentation
(defun kill-and-join-forward (&optional arg)
  "killing the newline between indented lines doesn’t remove any extra spaces caused by indentation."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))
(global-set-key (kbd "C-k") 'kill-and-join-forward)


(provide 'init-indent)