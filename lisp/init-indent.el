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

;; make sure using tab/space to indent
;; START TABS CONFIG
;; Create a variable for our preferred tab width
(setq custom-indent-width 2)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () 
  (progn
    (setq-default indent-tabs-mode nil)
    (setq indent-tabs-mode nil)))

(defun enable-tabs  ()
  (progn
    ;; (local-set-key (kbd "TAB") 'tab-to-tab-stop)
    (setq-default tab-width custom-indent-width)
    (setq tab-width custom-indent-width)
    (setq indent-tabs-mode t)))

(add-hook 'after-init-hook '(lambda ()
                              ;; Hooks to Enable Tabs
                              ;; (add-hook 'prog-mode-hook 'enable-tabs)
                              ;; (add-hook 'org-mode-hook 'enable-tabs)
                              (add-hook 'plantuml-mode-hook '(lambda ()
                                                               ;; plantuml seems always use tabs to do indent format
                                                               (enable-tabs)
                                                               (setq plantuml-indent-level custom-indent-width)))

                              ;; Hooks to Disable Tabs, since tab usually cause inconsistent visual appearence
                              (add-hook 'prog-mode-hook 'disable-tabs)
                              (add-hook 'org-mode-hook 'disable-tabs)
                              (add-hook 'json-mode-hook 'disable-tabs)
                              (add-hook 'lisp-mode-hook 'disable-tabs)
                              (add-hook 'emacs-lisp-mode-hook 'disable-tabs)
                              (add-hook 'yaml-mode-hook 'disable-tabs)

                              ;; Language-Specific Tweaks
                              (add-hook 'python-mode-hook '(lambda ()
                                                             (setq-default python-indent-offset custom-indent-width)
                                                             (setq python-indent-offset custom-indent-width)))))

;; (setq-default js-indent-level custom-indent-width)      ;; Javascript

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