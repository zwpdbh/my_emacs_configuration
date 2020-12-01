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
          'prettier-eslint-mode)

(provide 'init-prettier-eslint)
