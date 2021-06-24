;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; git submodule update --init --recursive
;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

;; Disable startup warning: "cl is deprecated"
(setq byte-compile-warnings '(cl-functions))

;; (setq my-emacs-root-directory "~/.emacs.d/")
;; (add-to-list 'load-path (expand-file-name "lisp" my-emacs-root-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'init-benchmarking)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))


;;----------------------------------------------------------------------------
;; Tune Emacs performance
;;----------------------------------------------------------------------------

;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
;;       (init-gc-cons-threshold (* 128 1024 1024)))
;;   (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; To test that both the fast JSON and native compilation is working
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (setq comp-deferred-compilation t)
      (message "Native compilation is available"))
  (message "Native complation is *not* available"))

;; And for the JSON
(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))



;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-const)
(require 'init-utils)     ;; the file provide useful common functions
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el 
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
;; Emacs Start Up Profiler
(unless (or (package-installed-p 'esup)
            (not window-system))  
  (package-install 'esup))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require-init 'init-autoload)

(maybe-require-package 'diminish)
(maybe-require-package 'scratch)
(maybe-require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)

;; set my favorite themes 
(require 'init-themes)

(require 'init-interface-tweaks)
(require 'init-gui-frames)
(require 'init-highlight-symbol)

(require-init 'init-dashboard)

(require-init 'init-helm)
(require-init 'init-counsel-ivy-swiper)
(require-init 'init-silver-search)
(require 'init-ripgrep)

(require 'init-selectrum)
(require 'init-orderless)
(require 'init-consult)
(require-init 'init-parenthese)


(require-init 'init-convenient)
(require-init 'init-all-the-icons)
(require-init 'init-clipboard)
(require-init 'init-keybinding)
(require-init 'init-whichkey)

(require 'init-font)

(require-init 'init-projectile)
;; (require-init 'init-treemacs)
(require-init 'init-ggtags)
(require-init 'init-markdown)
(require-init 'init-json)
(require-init 'init-docker)

(require 'init-dired)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-recentf)
(require 'init-hippie-expand)
(require 'init-windows)

(require 'init-editing-utils)
;; (require 'init-mmm)
(require 'init-vc)
(require 'init-git)


(require 'init-compile)
;; (require 'init-reveal)
(require-init 'init-package)
(require-init 'init-hydra)


;; === programing language related ===
(require-init 'init-company)
(require 'init-counsel-etags)
;; (require 'init-lsp)

(require 'init-terraform)
(require-init 'init-eldoc)

(require 'init-common-lisp-configure)

(require-init 'init-racket)
(require-init 'init-scheme)
(require-init 'init-clojure)
(require 'init-erlang)
(require 'init-ocaml)
(require 'init-elixir)
(require 'init-go)
(require-init 'init-lisp-tool)

(require-init 'init-clang-format)
(require-init 'init-yaml)
(require-init 'init-yasnippet)

;; (require-init 'init-python)
(require-init 'init-c-and-c++)
(require 'init-lua)
(require 'init-haskell)

(require-init 'init-javascript)
(require-init 'init-web)
(require 'init-vue)
(require 'init-prettier)
;; (require 'init-prettier-eslint)

(require-init 'init-R)

(require-init 'init-org)
(require-init 'init-org-babel)
(require-init 'init-org-tools)
(require-init 'init-org-html)
(require 'init-org-capture)
(require 'init-org-ob-browser)

(require-init 'init-plantuml)
(require-init 'init-mermaid)
(require-init 'init-graphviz)

(require 'init-sql)
(require 'init-ejc-sql)

(require 'init-folding)
(require 'init-indent)
(require 'init-electrify)
(require 'init-shell)
;; (require 'init-eshell) ; disable it for not using currently

;; (require-init 'init-pomodoro)

;; Tex related
(require-init 'init-tex)
;; (require 'init-bib)

;; ;; for Scimax is an Emacs starterkit for scientists and engineers. It provides a comprehensive configuration of Emacs for scientific programming and publishing.
;; (require 'init-scimax)

;; To add packages from local site-lisp folders
;; (add-to-list 'load-path
;;              "~/.emacs.d/site-lisp/<local_package_dir>")

;; ;;----------------------------------------------------------------------------
;; ;; emacs server related
;; ;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; define function to shutdown emacs server instance
(defun zw/emacs-server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))
