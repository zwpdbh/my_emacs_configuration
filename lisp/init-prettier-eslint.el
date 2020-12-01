;; prettier is the tool used to format code, but the formatted code may not pass eslint

;; eslint --fix is used to auto-format/fix much of code

;; prettier-eslint is a tool: https://github.com/prettier/prettier-eslint which format javascript project using prettier followed by
;; eslint --fix

;; prettier-eslint-cli is the CLI for prettier-eslint

;; Now, we use prettier-eslint-cli in emacs with the help from: https://github.com/ProjectFrank/prettier-eslint-emacs
(add-to-list 'load-path
             "~/.emacs.d/site-lisp/prettier-eslint-emacs")
(require 'prettier-eslint)

(setq prettier-eslint-args '(
                             "--trailing-comma" "all"
                             "--bracket-spacing" "false"
                             ))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()                             
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-eslint-mode))))

(add-hook 'vue-mode-hook
          '(lambda ()
             (prettier-eslint-mode)
             (add-hook 'before-save-hook
                       'prettier-eslint nil 'local)))

(provide 'init-prettier-eslint)
