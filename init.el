;; If it is fresh Emacs on Windows. Use the following setting to customize a different location for Emacs configuration
;; ;; Place ".emacs" file in C:\Users\Username\AppData\Roaming and point to the appropriate files
;; (setq user-init-file "D:/dev/.emacs.d/init.el")
;; (setq user-emacs-directory "D:/dev/.emacs.d/")
;; (setq default-directory "D:/dev/")
;; (setenv "HOME" "D:/dev/")
;; (load user-init-file)
;; Then create "dev" folder in section D and create sub-folder code inside "dev"
;; Don't forget to copy/move .ssh keys from actual home folder of your windows to "D:/dev/"

;; To fix error: No version of gnu-elpa-keyring-update >= nil is available
(when (version<= emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; To solve problem: Signature made by expired key 474F05837FBDEF9B GNU ELPA Signing Agent (2014) <elpasign@elpa.gnu.org>
(setq package-check-signature nil)

(load "~/.emacs.d/my-init.el")
(put 'narrow-to-region 'disabled nil)
;; ;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ;; ## end of OPAM user-setup addition for emacs / base ## keep this line
