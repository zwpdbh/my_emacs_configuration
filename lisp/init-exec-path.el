;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'exec-path-from-shell)

(if *win64*
    (progn
      (princ "set windows exec-path")
      (when (string-match "x86_64-w64-mingw" (emacs-version))
        (setq exec-path (append exec-path '("c:/tools/msys64/usr/bin")))))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(setq default-directory "~/code/")

(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (or (memq window-system '(mac ns x))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
