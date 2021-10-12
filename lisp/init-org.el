(use-package org
  :defer t
  :ensure org-plus-contrib)

;; To bind a key in a mode, you need to wait for the mode to be loaded before defining the key.
(after-load 'org
  (require 'ob)
  (require 'ob-js)
  (require 'org-eldoc)
  (require 'org-tempo)
  (require 'org-table)
  
  (global-set-key (kbd "<f12>") (kbd "C-c '"))
  (define-key org-mode-map [f5] #'org-toggle-inline-images)
  (define-key org-mode-map [f8] #'org-set-tags-command)
  (define-key org-mode-map [f11] #'org-toggle-narrow-to-subtree)
  (define-key org-mode-map (kbd "M-<return>") #'org-insert-item)

  ;; === org agenda
  ;; In case some org files is not listed in agenda files, run the code block again to refresh the file list.
  ;; Another way is to invoke the function ~org-agenda-file-to-front~.
  ;; make org-agenda to search all the TODOs recursively for files .org in folder "~/code/org/"
  ;; To update org-agenda-files, just delete outdated cache configuration from init.el
  ;; To add any current buffer into agenda files, use: "C-c [" which is "org-agenda-file-to-front"
  (setq org-agenda-files (directory-files-recursively "~/code/capture-org/" "\\`[^.].*\\.org\\'")
        org-tags-match-list-sublevels 'indented
        org-use-tag-inheritance nil)
  
  ;; Code run from org-mode-hook is for buffer-specific things which means code is evaluated for every org buffer.
  (add-hook 'org-mode-hook '(lambda ()
                              ;; set org to how to arrange current window when edit src code
                              (setq org-src-window-setup 'current-window)
                              
                              (setq org-log-done t)
                              (setq org-odt-preferred-output-format "docx"))))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)


(provide 'init-org)