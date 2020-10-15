(add-to-list 'load-path
             "~/.emacs.d/site-lisp/counsel-etags")

(require 'counsel-etags)


(defun zw/counsel-etags-list-tag-at-point ()
  "List tag at point, case insensitively"
  (interactive)
  (counsel-etags-tags-file-must-exist)

  (let* ((tagname (counsel-etags-tagname-at-point))
         (context (counsel-etags-execute-collect-function)))
    (cond
     (tagname
      (counsel-etags-find-tag-api tagname t buffer-file-name))
     (t
      (counsel-etags-find-tag-api nil t buffer-file-name)))))


(defun zw/counsel-etags-grep-at-point ()
  (interactive)
  (counsel-etags-grep (counsel-etags-tagname-at-point) nil nil t))


;; adjust key-bindings for counsel-etags
(defun zw/counsel-etags-key-bindings ()
  (interactive)
  ;; (define-key (current-local-map) (kbd "M-.") 'counsel-etags-find-tag-at-point)
  (define-key (current-local-map) (kbd "M-.") 'zw/counsel-etags-list-tag-at-point)
  (define-key (current-local-map) (kbd "M-/") 'zw/counsel-etags-grep-at-point))


;; Setup auto update now
(setq zw/use-counsel-etags-modes '(js-mode
                                   typescript-mode
                                   python-mode
                                   sh-mode
                                   c-mode
                                   c++-mode
                                   yaml-mode
                                   json-mode))

(dolist (each-mode zw/use-counsel-etags-modes)
  (add-hook (intern (format "%s-hook" each-mode))
            '(lambda ()
               (zw/counsel-etags-key-bindings)
               (add-hook 'after-save-hook
                         'counsel-etags-virtual-update-tags 'append 'local))))

;; Ignore directories and files
(with-eval-after-load 'counsel-etags
  (setq tags-add-tables t)
  (setq imenu-create-index-function 'counsel-etags-imenu-default-create-index-function)

  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  (setq counsel-etags-update-interval 30)
  
  ;; counsel-etags-ignore-directories does NOT support wildcast
  (add-to-list 'counsel-etags-ignore-directories "dist")
  (add-to-list 'counsel-etags-ignore-directories "build_clang")
  (add-to-list 'counsel-etags-ignore-directories "node_modules")
  
  ;; counsel-etags-ignore-filenames supports wildcast
  (add-to-list 'counsel-etags-ignore-filenames "TAGS")
  (add-to-list 'counsel-etags-ignore-filenames "package-lock.json")
  (add-to-list 'counsel-etags-ignore-filenames "#*")
  (add-to-list 'counsel-etags-ignore-filenames "#*.*#")
  (add-to-list 'counsel-etags-ignore-filenames "*.*#"))

(provide 'init-counsel-etags)