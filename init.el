;; To fix error: No version of gnu-elpa-keyring-update >= nil is available
(when (version<= emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(load "~/.emacs.d/my-init.el")
(put 'narrow-to-region 'disabled nil)
;; ;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ;; ## end of OPAM user-setup addition for emacs / base ## keep this line
