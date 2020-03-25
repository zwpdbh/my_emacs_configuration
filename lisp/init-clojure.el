(use-package cider
  :init
  ;; (setq cider-jack-in-default 'lein)
  (setq cider-jack-in-default 'boot)
  ;; (setq cider-default-cljs-repl 'Weasel)

  (if (string-equal system-type "windows-nt")
      (add-to-list 'exec-path "c:/ProgramData/chocolatey/bin/")
    nil)
  :commands (cider)
  :ensure t)

(use-package helm-cider
  :after (cider helm)
  :ensure t
  :init
  (add-hook 'cider-repl-mode-hook #'helm-cider-mode))

(use-package clj-refactor
  :ensure t
  :diminish clj-refactor-mode
  :after (clojure-mode))

(use-package clojure-mode
  :defer t
  :ensure t
  :config
  (progn
    (setq clojure-align-forms-automatically t)
    ;; In order for Emacs to recognise .boot files as valid Clojure source code
    (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
    (add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
    (add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojurescript-mode))

    (add-hook 'clojure-mode-hook
              '(lambda ()
                 ;; To properly indent hoplon macros. 
                 ;; Hoplon functions and macros
                 (dolist (pair '((page . 'defun)
                                 (loop-tpl . 'defun)
                                 (if-tpl . '1)
                                 (for-tpl . '1)
                                 (case-tpl . '1)
                                 (cond-tpl . 'defun)))
                   (put-clojure-indent (car pair)
                                       (car (last pair))))
                 ;; See documentation clojure-mode for specific indentations
                 (put-clojure-indent '>defn 2)
                 (clj-refactor-mode 1)
                 (cljr-add-keybindings-with-prefix "C-c C-/")))))

(setq org-babel-clojure-backend 'cider)
