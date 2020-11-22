;; To fix error: No version of gnu-elpa-keyring-update >= nil is available
(when (version<= emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(load "~/.emacs.d/my-init.el")
(put 'narrow-to-region 'disabled nil)
