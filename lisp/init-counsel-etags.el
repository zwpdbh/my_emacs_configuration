(when (maybe-require-package 'counsel-etags)
  (setq imenu-create-index-function 'counsel-etags-imenu-default-create-index-function)

  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  
  ;; Setup auto update now
  (setq zw/use-counsel-etags-modes '(js-mode
                                     python-mode
                                     sh-mode))
  (dolist (each-mode zw/use-counsel-etags-modes)
    (add-hook (intern (format "%s-hook" each-mode))
              '(lambda ()
                 (zw/customize-xref-key-bindings)
                 (add-hook 'after-save-hook
                           'counsel-etags-virtual-update-tags 'append 'local))))
  
  ;; Ignore directories and files
  (with-eval-after-load 'counsel-etags
    ;; counsel-etags-ignore-directories does NOT support wildcast
    (push "build_clang" counsel-etags-ignore-directories)
    (push "node_modules" counsel-etags-ignore-directories)
    ;; counsel-etags-ignore-filenames supports wildcast
    (push "TAGS" counsel-etags-ignore-filenames)
    (push "#*" counsel-etags-ignore-filenames)))

(provide 'init-counsel-etags)