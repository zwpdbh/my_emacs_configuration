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
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

;; (use-package ivy-rich
;;   :ensure t
;;   :init (ivy-rich-mode 1)
;;   :diminish (ivy-rich-mode)
;;   :config
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))


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
