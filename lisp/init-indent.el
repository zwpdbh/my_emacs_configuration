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
    (add-hook 'sgml-mode-hook 'indent-guide-mode)))


;; make sure using tab/space to indent
;; START TABS CONFIG
;; Create a variable for our preferred tab width
;; Make them different to indicate: indent 2 is using spaces, indent 4 is using tabs
(setq-default tab-width 4) 
(setq zw/indent-width 2)
(setq-default indent-tabs-mode nil)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun zw/disable-tabs ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun zw/enable-tabs  ()
  (interactive)
  (setq indent-tabs-mode t))

(add-hook 'after-init-hook '(lambda ()
                              ;; All the mode in which indentation could insert tabs
                              ;; Hooks to Enable Tabs
                              (add-hook 'plantuml-mode-hook '(lambda ()
                                                               ;; plantuml seems always use tabs to do indent format
                                                               (zw/enable-tabs)
                                                               (setq plantuml-indent-level zw/indent-width)))

                              ;; All the mode in which indentation could not insert tabs
                              ;; Hooks to Disable Tabs, since tab usually cause inconsistent visual appearence
                              (add-hook 'prog-mode-hook 'zw/disable-tabs)
                              (add-hook 'org-mode-hook 'zw/disable-tabs)
                              (add-hook 'json-mode-hook 'zw/disable-tabs)
                              (add-hook 'lisp-mode-hook 'zw/disable-tabs)
                              (add-hook 'emacs-lisp-mode-hook 'zw/disable-tabs)
                              (add-hook 'yaml-mode-hook 'zw/disable-tabs)
                              ;; Language-Specific Tweaks
                              (add-hook 'python-mode-hook '(lambda ()
                                                             ;; (set (make-local-variable 'zw/indent-width) 4)
                                                             (setq-local zw/indent-width 4)
                                                             (setq python-indent-offset zw/indent-width)))))

;; (setq-default js-indent-level zw/indent-width)      ;; Javascript

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit nil)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; ;; WARNING: This will change your life
;; ;; (OPTIONAL) Visualize tabs as a pipe character - "|"
;; ;; This will also show trailing characters as they are useful to spot.
;; (setq whitespace-style '(face tabs tab-mark trailing))
;; (custom-set-faces
;;  '(whitespace-tab ((t (:foreground "#636363")))))
;; (setq whitespace-display-mappings
;;   '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
;; (global-whitespace-mode) ; Enable whitespace mode everywhere

;; END TABS CONFIG


(provide 'init-indent)