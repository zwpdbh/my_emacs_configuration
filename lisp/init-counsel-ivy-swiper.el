;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t)

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

(use-package swiper
  :ensure try
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ;; ("C-x C-f" . counsel-find-file)
         ;; ("M-x" . counsel-M-x)
         )
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))


(provide 'init-counsel-ivy-swiper)
