(add-to-list 'load-path
             "~/.emacs.d/site-lisp/counsel-etags")

(require 'counsel-etags)

;; adjust key-bindings for counsel-etags
(defun zw/counsel-etags-key-bindings ()
  (interactive)
  (define-key (current-local-map) (kbd "M-.") 'counsel-etags-find-tag-at-point)
  (define-key (current-local-map) (kbd "M-/") 'zw/counsel-etags-grep-at-point))

(defun zw/counsel-etags-grep-at-point (&optional hint root)
  (interactive)
  (let* ((text (counsel-etags-tagname-at-point))
         (keyword (funcall counsel-etags-convert-grep-keyword text))
         (default-directory (file-truename (or root
                                               (counsel-etags-locate-project))))
         (time (current-time))
         (cmd (counsel-etags-grep-cli keyword nil))
         (cands (split-string (shell-command-to-string cmd) "[\r\n]+" t))
         (dir-summary (counsel-etags-dirname default-directory)))

    (when (and cands
               buffer-file-name
               counsel-etags-sort-grep-result-p
               counsel-etags-candidates-optimize-limit
               ;; string-distance is faster
               (< (length cands) (* 4 counsel-etags-candidates-optimize-limit))
               (fboundp 'string-distance))
      ;; grep should not waste time on lisp version of string distance
      ;; So `string-distance' from Emacs 27 is required
      (let* ((ref (file-relative-name buffer-file-name root)))
        (setq cands
              (sort cands
                    `(lambda (a b)
                       (< (string-distance (car (split-string a ":")) ,ref t)
                          (string-distance (car (split-string b ":")) ,ref t)))))))

    (if counsel-etags-debug (message "counsel-etags-grep called => %s %s %s %s"
                                     keyword default-directory cmd cands))
    (counsel-etags-put :ignore-dirs
                       counsel-etags-ignore-directories
                       counsel-etags-opts-cache)

    (counsel-etags-put :ignore-file-names
                       counsel-etags-ignore-filenames
                       counsel-etags-opts-cache)

    ;; Slow down grep 10 times
    (ivy-read (concat hint (format "Grep \"%s\" at %s (%s): "
                                   text
                                   dir-summary
                                   (counsel-etags--time-cost time)))
              cands
              :history 'counsel-git-grep-history ; share history with counsel
              :action `(lambda (item)
                         ;; when grepping, we grepping in project root
                         (counsel-etags-open-file-api item
                                                      ,default-directory
                                                      ,keyword))
              :caller 'counsel-etags-grep)))

;; Setup auto update now
(setq zw/use-counsel-etags-modes '(js-mode
                                   python-mode
                                   sh-mode
                                   c-mode
                                   c++-mode
                                   yaml-mode
                                   json-mode))

(dolist (each-mode zw/use-counsel-etags-modes)
  (add-hook (intern (format "%s-hook" each-mode))
            '(lambda ()
               (zw/counsel-etags-key-bindings)
               (add-hook 'after-save-hook
                         'counsel-etags-virtual-update-tags 'append 'local))))

;; Ignore directories and files
(with-eval-after-load 'counsel-etags
  (setq tags-add-tables t)
  (setq imenu-create-index-function 'counsel-etags-imenu-default-create-index-function)

  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  
  (setq counsel-etags-update-interval 30)
  ;; counsel-etags-ignore-directories does NOT support wildcast
  (push "build_clang" counsel-etags-ignore-directories)
  (push "node_modules" counsel-etags-ignore-directories)
  ;; counsel-etags-ignore-filenames supports wildcast
  (push "TAGS" counsel-etags-ignore-filenames)
  (push "#*" counsel-etags-ignore-filenames))

(provide 'init-counsel-etags)