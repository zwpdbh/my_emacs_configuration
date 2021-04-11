
;; we still need theme because currently counsel-etags need theme to work properly
;; it looks like counsel is a requirement for swiper
(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t)

(use-package swiper
  :ensure t)

(when (maybe-require-package 'swiper)
  (global-set-key (kbd "C-s") 'swiper))


(provide 'init-counsel-ivy-swiper)
