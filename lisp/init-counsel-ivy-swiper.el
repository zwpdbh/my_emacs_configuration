;; We still need all three of them because currently counsel-etags need theme to work properly
;; It looks like counsel is a requirement for swiper, and otherwise show counsel not found error.
;; These 3 packages works together. Do not forget to install any of them.

(maybe-require-package 'counsel)
(maybe-require-package 'swiper)

(when (maybe-require-package 'ivy)
  (when (maybe-require-package 'ivy-xref)
    ;; xref initialization is different in Emacs 27 - there are two different
    ;; variables which can be set rather than just one
    (when (>= emacs-major-version 27)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
    ;; commands other than xref-find-definitions (e.g. project-find-regexp)
    ;; as well
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))


(global-set-key (kbd "M-x") 'counsel-M-x)


(provide 'init-counsel-ivy-swiper)
