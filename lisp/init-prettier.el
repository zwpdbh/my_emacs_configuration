;; npm install prettier --save-dev
;; or
;; sudo npm install -g prettier for global usage

;; prettier is the tool used to format code
;; The prettier(prettier.el) Emacs package reformats your code by running Prettier with minimal overhead, by request or transparently on file save.
(when (maybe-require-package 'prettier)
  ;; format code for web-mode before save
  (when (fboundp 'prettier)
    (add-hook 'web-mode-hook
              '(lambda ()
                 (add-hook 'before-save-hook
                           'prettier-prettify nil 'local))))


  (add-hook 'sgml-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook
                         'prettier-prettify nil 'local))))


(provide 'init-prettier)