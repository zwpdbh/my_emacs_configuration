;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :init 
  (ivy-mode 1)
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "%d/%d ")
    (setq ivy-display-style 'fancy)))

(when (maybe-require-package 'ivy-xref)
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-rich
  :ensure t
  :init
  :diminish (ivy-rich-mode)
  :config
  (progn
    (defun ivy-rich-switch-buffer-icon (candidate)
      (with-current-buffer
          (get-buffer candidate)
        (let ((icon (all-the-icons-icon-for-mode major-mode)))
          (if (symbolp icon)
              (all-the-icons-icon-for-mode 'fundamental-mode)
            icon))))
    
    (setq ivy-rich-path-style 'abbrev)
    (setq ivy-rich-display-transformers-list
          '(ivy-switch-buffer
            (:columns
             ((ivy-rich-switch-buffer-icon (:width 2))
              ;; add face by the original transformer
              (ivy-switch-buffer-transformer (:width 60))    
              ;; (ivy-rich-switch-buffer-size (:width 7))  ; return buffer size
              ;; (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))  ; return buffer indicator
              ;; (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))            ; return major mode info
              (ivy-rich-switch-buffer-project (:width 0.2 :face success))               ; return project name `projectile'
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
             :predicate
             (lambda (cand) (get-buffer cand)))
            counsel-find-file
            (:columns
             ((ivy-read-file-transformer)
              (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
            counsel-M-x
            (:columns
             ((counsel-M-x-transformer (:width 40))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))) ; return docstring of the command
            counsel-describe-function
            (:columns
             ((counsel-describe-function-transformer (:width 40))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return docstring of the function
            counsel-describe-variable
            (:columns
             ((counsel-describe-variable-transformer (:width 40))
              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return docstring of the variable
            counsel-recentf
            (:columns
             ((ivy-rich-candidate (:width 0.8))
              (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))  ; return last modified time of the file
            package-install
            (:columns
             ((ivy-rich-candidate (:width 30))
              (ivy-rich-package-version (:width 16 :face font-lock-comment-face))  ; return package version
              (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))  ; return archive summary
              ;; return package description
              (ivy-rich-package-install-summary (:face font-lock-doc-face))))))
    ;; (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    (setq ivy-rich-path-style 'abbrev)
    (ivy-rich-mode 1)))


(use-package swiper
  :ensure try
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ;; ("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))


(provide 'init-counsel-ivy-swiper)
