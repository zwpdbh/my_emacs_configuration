
(when (maybe-require-package 'orderless)
  (after-load 'selectrum
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))
  
  (after-load 'company
    (setq orderless-component-separator "[ &]")
    (defun just-one-face (fn &rest args)
      (let ((orderless-match-faces [completions-common-part]))
        (apply fn args)))
    (advice-add 'company-capf--candidates :around #'just-one-face)))

(provide 'init-orderless)