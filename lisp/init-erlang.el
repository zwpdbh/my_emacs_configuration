;; install erlang
;; https://medium.com/erlang-central/erlang-quick-install-9c5dcaa5b634
;; on windows: https://www.erlang.org/downloads
;; on ubuntu: sudo apt install erlang

;; emacs configruation refs:
;; http://alexott.net/en/writings/emacs-devenv/EmacsErlang.html
;; https://www.lambdacat.com/post-modern-emacs-setup-for-erlang/
;; https://seancribbs.com/emacs.d#sec-6-3

;; The OTP emacs mode doesn't come with some of my commonly used files that are Erlang code or terms.
(add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("rebar.config.script" . erlang-mode))
(add-to-list 'auto-mode-alist '("app.config" . erlang-mode))
(add-to-list 'auto-mode-alist '("lib_chan.conf" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.erlang$" . erlang-mode))


;; When Erlang has been installed, it by default contains elisp code for Emacs to use.
;; Configure to load those .el files for erlang-mode

(defun get-latest-erlang-lib-tools-dir ()
  (let ((erlang-lib-dir (concat erlang-root-dir "lib/")))
    (seq-filter (apply-partially #'< 3) '(1 2 3 4 5 6))
    (let ((tools (seq-filter (apply-partially #'(lambda (str)
                                                  (and (string-match "^tools" str)
                                                       (file-exists-p (concat erlang-root-dir "lib/" str "/")))))
                             (directory-files erlang-lib-dir))))
      (nth (1- (length tools)) tools))))

(cond (*win64*
       (setq erlang-root-dir "c:/Program Files/erl-23.0/")
       (setq erlang-man-root-dir (concat erlang-root-dir "man"))
       (add-to-list 'exec-path (concat erlang-root-dir "bin/"))
       (add-to-list 'load-path (concat erlang-root-dir "lib/" (get-latest-erlang-lib-tools-dir) "/emacs")))
      (*is-a-mac*
       (setq erlang-root-dir "/usr/local/lib/erlang/")
       (setq erlang-man-root-dir (concat erlang-root-dir "man"))
       (add-to-list 'exec-path (concat erlang-root-dir "bin"))
       (add-to-list 'load-path (concat erlang-root-dir "lib/" (get-latest-erlang-lib-tools-dir) "/emacs")))
      (t
       (setq erlang-root-dir "/usr/lib/erlang/")
       (setq erlang-man-root-dir (concat erlang-root-dir "man"))
       (add-to-list 'exec-path (concat erlang-root-dir "bin"))
       (add-to-list 'load-path (concat erlang-root-dir "lib/" (get-latest-erlang-lib-tools-dir) "/emacs"))))


(when (executable-find "erl")
  ;; If this part triggers error, check erlang-root-dir settings.
  (require 'erlang-start)
  (setq inferior-erlang-machine-options '("-sname" "emacs")))


(defun my/disable-paredit-spaces-before-paren ()
  ;; Function which always returns nil -> never insert a space when insert a parentheses.
  (defun my/erlang-paredit-space-for-delimiter-p (endp delimiter) nil)
  ;; Set this function locally as only predicate to check when determining if a space should be inserted
  ;; before a newly created pair of parentheses.
  (setq-local paredit-space-for-delimiter-predicates '(my/erlang-paredit-space-for-delimiter-p)))

;; ;; TODO:: not very powerful, try it after feel the limitation
;; ;; ref: https://github.com/s-kostyaev/ivy-erlang-complete
;; (when (maybe-require-package 'ivy-erlang-complete))
;; (when (maybe-require-package 'company-erlang))

(add-hook 'erlang-mode-hook
          '(lambda ()
             (my/disable-paredit-spaces-before-paren)
             (paredit-mode t)
             ;; ;; configuration for ivy-erlang-complete with company-erlang
             ;; (setq-local company-backends (zw/add-to-company-backends 'company-erlang))
             ;; (add-hook 'after-save-hook 'ivy-erlang-complete-reparse 'append 'local)

             ;; ;; key-bindings for ivy-erlang-complete
             ;; (define-key erlang-mode-map (kbd "M-.") 'ivy-erlang-complete-find-definition)
             ;; (define-key erlang-mode-map (kbd "M-/") 'ivy-erlang-complete-find-references)
             ;; (define-key erlang-mode-map (kbd "C-c C-f") 'ivy-erlang-complete-find-spec)
             ;; (define-key erlang-mode-map (kbd "C-c C-o") 'ivy-erlang-complete-find-file)

             ;; my own simple solution to format erlang code on save
             (add-hook 'before-save-hook 'erlang-indent-clause nil 'local)
             ;; key-bindings for erlang-mode
             (define-key erlang-mode-map (kbd "C-c C-c") 'erlang-compile)))

;; ;; TODO:: could not compile the downloaded package
;; ;; Tracking issue: https://github.com/massemanet/distel/issues/70
;; ;; ref: http://alexott.net/en/writings/emacs-devenv/EmacsErlang.html
;; ;; Installation & customization of distel
;; (after-load 'erlang
;;   (add-to-list 'load-path "~/.emacs.d/site-lisp/distel/elisp")
;;   (require 'distel)
;;   (distel-setup))



;; Support LFE(lisp-flavoured-erlang)
;; As LFE installed, it ships with .el configuration for Emacs. 
(setq lfe-dir (concat (getenv "HOME") "/.cache/rebar3/plugins/lfe"))
(add-to-list 'load-path (concat lfe-dir "/emacs"))
;; add lfe execution file
(add-to-list 'exec-path (concat lfe-dir "/bin"))

(when (load "lfe-mode.el" t)
  (add-to-list 'auto-mode-alist '("\\.lfe$" . lfe-mode))
  (require 'lfe-start)

  ;; ref: https://erickgnavar.github.io/emacs-config/#org092dac2
  ;; sexp eval for LFE-mode
  (defun zw/lfe-eval-buffer ()
    "Send current buffer to inferior LFE process."
    (interactive)
    (if (eq (get-buffer-window "*inferior-lfe*") nil)
        (run-lfe nil))
    (lfe-eval-region (point-min) (point-max) nil))
  
  ;; Copied from default lfe-eval-last-sexp
  (defun zw/lfe-eval-last-sexp (&optional and-go)
    "Send the previous sexp to the inferior LFE process. `AND-GO' means switch to the LFE buffer afterwards."
    (interactive "P")
    (if (eq (get-buffer-window "*inferior-lfe*") nil)
        (run-lfe nil))
    (lfe-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))
  
  (define-key lfe-mode-map (kbd "C-c C-e") 'zw/lfe-eval-buffer)  
  (define-key lfe-mode-map (kbd "C-c C-c") 'zw/lfe-eval-last-sexp)
  
  (after-load 'org
    (defun org-babel-execute:lfe (body params) body)
    (add-to-list 'org-structure-template-alist '("lfe" . "src lfe"))))


(after-load 'org
  ;; (add-to-list 'load-path
  ;;              "~/.emacs.d/site-lisp/ob-erlang")
  ;; (require 'ob-erlang)
  (add-to-list 'zw/org-babel-evaluate-whitelist "erlang")
  (add-to-list 'zw/org-babel-load-language-list '(erlang . t))
  (add-to-list 'org-structure-template-alist '("erlang" . "src erlang")))


(add-hook 'erlang-shell-mode-hook
          (lambda ()
            (my/disable-paredit-spaces-before-paren)
            (paredit-mode t)
            (setq-local company-backends '((company-dabbrev company-capf) company-keywords company-files))))


(provide 'init-erlang)
;;; init-erlang ends here
