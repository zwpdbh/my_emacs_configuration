(when (maybe-require-package 'orderless)
  (after-load 'selectrum
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)))

(provide 'init-orderless)