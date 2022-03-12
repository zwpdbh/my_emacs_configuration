;; We still need all three of them because currently counsel-etags need theme to work properly
;; It looks like counsel is a requirement for swiper, and otherwise show counsel not found error.
;; These 3 packages works together. Do not forget to install any of them.

(maybe-require-package 'counsel)
(maybe-require-package 'swiper)
(maybe-require-package 'ivy)
(global-set-key (kbd "M-x") 'counsel-M-x)


(provide 'init-counsel-ivy-swiper)
