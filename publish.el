(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

(require 'init-const)
(require 'init-utils)     ;; the file provide useful common functions
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el 
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; load org export related configuration
(require 'init-org)
(require 'init-org-babel)
(require 'init-org-html)

(require 'org)
(require 'ob)
(require 'ob-js)
(require 'org-eldoc)
(require 'org-tempo)
(require 'org-table)

(if (member "t" command-line-args)
    (progn
      (print "force publish all org files")
      (org-publish-all t)
      )
    (progn
      (print "only publish modified org files")
      (org-publish-all)))