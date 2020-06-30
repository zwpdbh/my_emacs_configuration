;;; init-erlang.el --- Support with erlang -*- lexical-binding: t -*-

;; install erlang
;; https://medium.com/erlang-central/erlang-quick-install-9c5dcaa5b634
;; on windows: https://www.erlang.org/downloads
;; on ubuntu: sudo apt install erlang

;; emacs configruation refs:
;; http://alexott.net/en/writings/emacs-devenv/EmacsErlang.html
;; https://www.lambdacat.com/post-modern-emacs-setup-for-erlang/
;; https://seancribbs.com/emacs.d#sec-6-3


(cond (*win64*
       (add-to-list 'load-path "c:/Program Files/erl-23.0/lib/tools-3.4/emacs")
       (add-to-list 'exec-path "c:/Program Files/erl-23.0/bin")
       (setq erlang-root-dir "c:/Program Files/erl-23.0"))
      (t
       (add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.11.1/emacs")
       (add-to-list 'exec-path "/usr/bin")
       (add-to-list 'exec-path "/usr/local/bin")
       (setq erlang-root-dir "/usr/local/otp")))


(when (executable-find "erl")
  (require 'erlang-start)
  ;; The OTP emacs mode doesn't come with some of my commonly used files that are Erlang code or terms.
  (add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode))
  (add-to-list 'auto-mode-alist '("rebar.config.script" . erlang-mode))
  (add-to-list 'auto-mode-alist '("app.config" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.erlang$" . erlang-mode)))




(provide 'init-erlang)
;;; init-erlang ends here