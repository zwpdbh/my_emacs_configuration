;; npm install prettier --save-dev

;; prettier is the tool used to format code
;; The prettier(prettier.el) Emacs package reformats your code by running Prettier with minimal overhead, by request or transparently on file save.
(when (maybe-require-package 'prettier)
  
  ;; format code for vue-mode before save 
  (add-hook 'vue-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook
                         'prettier-prettify nil 'local))))

(provide 'init-prettier)