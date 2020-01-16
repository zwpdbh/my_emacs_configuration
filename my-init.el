;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

;; (setq my-emacs-root-directory "~/.emacs.d/")
;; (add-to-list 'load-path (expand-file-name "lisp" my-emacs-root-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-const)
(require 'init-utils)
;; (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
;; (require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-recentf)

(require 'init-hippie-expand)
(require 'init-windows)
(require 'init-mmm)

(require 'init-editing-utils)
;; (require 'init-whitespace)

(require 'init-vc)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)

(require 'init-compile)

;; (require 'init-textile)
;; (require 'init-markdown)
;; (require 'init-csv)
;; (require 'init-erlang)
(require 'init-javascript)
(require 'init-company)
;; (require 'init-php)
;; (require 'init-org)
;; (require 'init-nxml)
;; (require 'init-html)
;; (require 'init-css)
;; (require 'init-haml)
;; (require 'init-http)
;; (require 'init-python)
;; (require 'init-haskell)
;; (require 'init-elm)
;; (require 'init-purescript)
;; (require 'init-ruby)
;; (require 'init-rails)
;; (require 'init-sql)
;; (require 'init-rust)
;; (require 'init-toml)
;; (require 'init-yaml)
;; (require 'init-docker)
;; (require 'init-terraform)
;; (require 'init-nix)
;; (maybe-require-package 'nginx-mode)

;; (require 'init-paredit)
;; (require 'init-lisp)
;; (require 'init-slime)
;; (require 'init-clojure)
;; (require 'init-clojure-cider)
;; (require 'init-common-lisp)

;; (when *spell-check-support-enabled*
;;   (require 'init-spelling))

;; (require 'init-misc)

;; (require 'init-folding)
;; (require 'init-dash)

;; ;;(require 'init-twitter)
;; ;; (require 'init-mu)
;; (require 'init-ledger)
;; ;; Extra packages which don't require any configuration

;; (require-package 'gnuplot)
;; (require-package 'lua-mode)
;; (require-package 'htmlize)
;; (require-package 'dsvn)
;; (when *is-a-mac*
;;   (require-package 'osx-location))
;; (unless (eq system-type 'windows-nt)
;;   (maybe-require-package 'daemons))
;; (maybe-require-package 'dotenv-mode)

;; (when (maybe-require-package 'uptimes)
;;   (setq-default uptimes-keep-count 200)
;;   (add-hook 'after-init-hook (lambda () (require 'uptimes))))

;; (when (fboundp 'global-eldoc-mode)
;;   (add-hook 'after-init-hook 'global-eldoc-mode))

;; ;;----------------------------------------------------------------------------
;; ;; Allow access from emacsclient
;; ;;----------------------------------------------------------------------------
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (require 'server)
;;             (unless (server-running-p)
;;               (server-start))))


(org-babel-load-file (expand-file-name "~/.emacs.d/my-extra-init.org"))
