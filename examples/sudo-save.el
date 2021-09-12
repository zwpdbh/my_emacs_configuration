;; https://www.emacswiki.org/emacs/SudoSave
;; From question: Open a file with su/sudo inside Emacs
(defun sudo-before-save-hook ()
  (set (make-local-variable 'sudo:file) (buffer-file-name))
  (when sudo:file
    (unless(file-writable-p sudo:file)
      (set (make-local-variable 'sudo:old-owner-uid) (nth 2 (file-attributes sudo:file)))
      (when (numberp sudo:old-owner-uid)
	      (unless (= (user-uid) sudo:old-owner-uid)
	        (when (y-or-n-p
		             (format "File %s is owned by %s, save it with sudo? "
			                   (file-name-nondirectory sudo:file)
			                   (user-login-name sudo:old-owner-uid)))
	          (sudo-chown-file (int-to-string (user-uid)) (sudo-quoting sudo:file))
	          (add-hook 'after-save-hook
		                  (lambda ()
			                  (sudo-chown-file (int-to-string sudo:old-owner-uid)
					                               (sudo-quoting sudo:file))
			                  (if sudo-clear-password-always
			                      (sudo-kill-password-timeout)))
		                  nil   ;; not append
		                  t	    ;; buffer local hook
		                  )))))))
(add-hook 'before-save-hook 'sudo-before-save-hook)