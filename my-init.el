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
;; (require 'init-benchmarking)

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
(require 'init-utils)     ;; the file provide useful common functions
(require 'init-elpa)      ;; Machinery for installing required packages

(unless (package-installed-p 'esup) ;; Emacs Start Up Profiler
  (package-install 'esup))

(require 'init-exec-path) ;; Set up $PATH
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(maybe-require-package 'diminish)
(maybe-require-package 'scratch)
(maybe-require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-recentf)
(require 'init-hippie-expand)
(require 'init-windows)
(require 'init-mmm)
(require 'init-editing-utils)
(require 'init-vc)
(require 'init-git)
(require 'init-github)
(require 'init-projectile)
(require 'init-compile)
(require 'init-reveal)
;; === programing language related
(require 'init-company)
(require 'init-lsp)
(require 'init-terraform)


;; ;;----------------------------------------------------------------------------
;; ;; Allow access from emacsclient
;; ;;----------------------------------------------------------------------------
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (require 'server)
;;             (unless (server-running-p)
;;               (server-start))))


(org-babel-load-file (expand-file-name "~/.emacs.d/my-extra-init.org"))
