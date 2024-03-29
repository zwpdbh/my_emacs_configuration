;; prevent backspace from deleting my shell prompt
(setq comint-prompt-read-only t)

;; It is used to customize shell settings during shell-mode
(defun zw/set-bash-profile ()
  (interactive)
  (progn
    (insert "source ~/.emacs.d/shell/bash_profile.sh")
    (comint-send-input nil t)))

;; "C-u M-x shell" could be used to start multiple shells
(add-hook 'shell-mode-hook
          #'(lambda ()
              (setq-local company-backends '((company-dabbrev company-keywords)))
              (zw/set-bash-profile)))

(add-hook 'sh-mode-hook
          #'(lambda ()
             (setq-local company-backends (zw/delete-from-company-backends 'company-capf))))

(provide 'init-shell)
