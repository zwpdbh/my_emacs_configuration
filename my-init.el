;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; git submodule update --init --recursive
;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

;; To set git config for this project:
;; cd ~/.emacs.d/
;; git config user.name zwpdbh
;; git config user.email hyperion_z@outlook.com

;; Disable startup warning: "cl is deprecated"
(setq byte-compile-warnings '(cl-functions))

;; To solve error: nesting exceeds `max-lisp-eval-depth'
(setq max-lisp-eval-depth 10000)

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

(require 'init-autoload)

(maybe-require-package 'diminish)
(maybe-require-package 'scratch)
(maybe-require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)

;; set my favorite themes
(require 'init-font)
(require 'init-themes)

(require 'init-interface-tweaks)
(require 'init-gui-frames)
(require 'init-highlight-symbol)

(require 'init-dashboard)

(require 'init-helm)
(require 'init-selectrum)
(require 'init-orderless)
(require 'init-consult)
(require 'init-projectile)
(require 'init-parenthese)


(require 'init-convenient)
(require 'init-clipboard)
(require 'init-keybinding)
;; (require 'init-whichkey)


;; (require 'init-treemacs)
(require 'init-markdown)
(require 'init-json)
;; (require 'init-docker)

(require 'init-dired)
;; (require 'init-grep)
(require 'init-uniquify)
;; (require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-recentf)
(require 'init-hippie-expand)
(require 'init-windows)

(require 'init-editing-utils)
;; (require 'init-mmm)
;; (require 'init-vc)
(require 'init-git)


(require 'init-compile)
;; (require 'init-reveal)
(require 'init-package)
(require 'init-hydra)


;; === programing language related ===
(require 'init-company)
(require 'init-counsel-ivy-swiper)
(require 'init-counsel-etags)

;; (require 'init-terraform)
(require 'init-eldoc)

(require 'init-lisp)
(require 'init-racket)
(require 'init-scheme)
;; (require 'init-clojure)
(require 'init-erlang)
(require 'init-ocaml)
(require 'init-fsharp)
(require 'init-elixir)
(require 'init-go)
(require 'init-lisp-tool)

(require 'init-clang-format)
(require 'init-yaml)
(require 'init-yasnippet)

;; (require 'init-python)
(require 'init-c-and-c++)
;; (require 'init-lua)
;; (require 'init-haskell)
(require 'init-csharp)

(require 'init-javascript)
(require 'init-web)
(require 'init-vue)
(require 'init-prettier)
;; (require 'init-prettier-eslint)

;; (require 'init-R)

;; (require 'init-lsp)
;; (require 'init-eglot)

(require 'init-org)
(require 'init-org-babel)
(require 'init-org-tools)
(require 'init-org-html)
(require 'init-org-capture)
(require 'init-org-ob-browser)

(require 'init-plantuml)
(require 'init-mermaid)
(require 'init-graphviz)

(require 'init-sql)
(require 'init-ejc-sql)

(require 'init-folding)
(require 'init-indent)
(require 'init-electrify)
(require 'init-shell)
;; (require 'init-eshell) ; disable it for not using currently

;; (require 'init-pomodoro)

;; ===Tex related
;; (require 'init-tex)
;; (require 'init-bib)

;; ;; for Scimax is an Emacs starterkit for scientists and engineers. It provides a comprehensive configuration of Emacs for scientific programming and publishing.
;; (require 'init-scimax)

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
