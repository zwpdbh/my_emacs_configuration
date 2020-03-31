(use-package org
  :init
  (setq org-link-file-path-type 'adaptive)
  :defer t
  :ensure org-plus-contrib)

;; To bind a key in a mode, you need to wait for the mode to be loaded before defining the key.
(eval-after-load 'org
  #'(lambda ()
      (require 'ob)
      (require 'ob-js)
      (require 'org-eldoc)
      (require 'org-tempo)
      
      (global-set-key (kbd "<f12>") (kbd "C-c '"))
      (define-key org-mode-map [f5] #'org-toggle-inline-images)
      (define-key org-mode-map [f11] #'org-toggle-narrow-to-subtree)))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(add-hook 'org-mode-hook '(lambda ()
                            ;; set org to user the current window when edit src code
                            (setq org-src-window-setup 'current-window)
                            (setq org-log-done t)))


;; === org agenda
;; In case some org files is not listed in agenda files, run the code block again to refresh the file list.
;; Another way is to invoke the function ~org-agenda-file-to-front~.
;; make org-agenda to search all the TODOs recursively for files .org in folder "~/code/org/"
(setq org-agenda-files (directory-files-recursively "~/code/capture-org/" "\\.org$"))

(provide 'init-org)