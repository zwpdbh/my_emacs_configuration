;; refs
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-LaTeX.html
;; http://www.stat.rice.edu/~helpdesk/compguide/node39.html

;;; ZW/Note
;; On windows10
;; Install MiKTex
;; Add its exe into system-path: "C:/tools/MiKTeX/miktex/bin/x64"
;; If met error when F5, which is save-compile-latex; run MiKTex console and updates its packages

;; On Ubuntu
;; sudo apt install auctex

(when (maybe-require-package 'auctex)
  (cond ((eq system-type 'darwin)
         (setq exec-path (append exec-path '("/Library/TeX/texbin/"))))))


(when (maybe-require-package 'reftex)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'reftex-mode-hook
            '(lambda ()
               ;; A function to delete the current macro in AUCTeX.
               ;; Note: keybinds won't be added to TeX-mode-hook if not kept at the end of the AUCTeX setup!
               (defun TeX-remove-macro ()
                 "Remove current macro and return TRUE, If no macro at point, return Nil."
                 (interactive)
                 (when (TeX-current-macro)
                   (let ((bounds (TeX-find-macro-boundaries))
                         (brace  (save-excursion
                                   (goto-char (1- (TeX-find-macro-end)))
                                   (TeX-find-opening-brace))))
                     (delete-region (1- (cdr bounds)) (cdr bounds))
                     (delete-region (car bounds) (1+ brace)))
                   t))
               (reftex-isearch-minor-mode t)
               (define-key TeX-mode-map (kbd "C-c C-r") 'reftex-query-replace-document)
               (define-key TeX-mode-map (kbd "M-<delete>") 'TeX-remove-macro)))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))


(after-load 'tex
  (defun save-compile-latex ()
    "Save and compile latex document"
    (interactive)
    (save-buffer)
    (TeX-command-sequence t t))

  (defun complete-if-no-space ()
    (interactive)
    (let ((cb (string (char-before))))
      (if (or (equal cb " ") (equal (point) (line-beginning-position)))
          (tab-to-tab-stop)
        (TeX-complete-symbol))))
  
  ;; Set pdf tool to open preview
  ;; On windows10"C:\Program Files\SumatraPDF\SumatraPDF.exe"
  (when *win64*
    (setq TeX-view-program-list
          '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                            (mode-io-correlate " -forward-search %b %n ") " %o"))))
    (setq TeX-view-program-selection '((output-pdf "Sumatra PDF"))))
  (when *linux*
    (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)) 
          TeX-view-program-selection '((output-pdf "PDF Tools"))))

  (setq-default TeX-master nil ; by each new fie AUCTEX will ask for a master fie.
                TeX-PDF-mode t
                TeX-engine 'xetex)     ; optional

  ;; (setq LaTeX-indent-environment-list
  ;;       '(("itemize" LaTeX-indent-tabular)
  ;;         ("enumerate" LaTeX-indent-tabular)
  ;;         ("verbatim" current-indentation)
  ;;         ("verbatim*" current-indentation)
  ;;         ("tabular" LaTeX-indent-tabular)
  ;;         ("tabular*" LaTeX-indent-tabular)
  ;;         ("align" LaTeX-indent-tabular)
  ;;         ("align*" LaTeX-indent-tabular)
  ;;         ("array" LaTeX-indent-tabular)
  ;;         ("eqnarray" LaTeX-indent-tabular)
  ;;         ("eqnarray*" LaTeX-indent-tabular)
  ;;         ("multline" LaTeX-indent-tabular)
  ;;         ("displaymath")
  ;;         ("equation")
  ;;         ("equation*")
  ;;         ("picture")
  ;;         ("tabbing")))
  
  (setq TeX-auto-save t
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
        ;; TeX-save-query nil       ; don't prompt for saving the .tex file
        ;; TeX-show-compilation nil         ; if `t`, automatically shows compilation log
        ;; LaTeX-babel-hyphen nil ; Disable language-specific hyphen insertion.
        ;; ;; `"` expands into csquotes macros (for this to work, babel pkg must be loaded after csquotes pkg).
        ;; LaTeX-csquotes-close-quote "}"
        ;; LaTeX-csquotes-open-quote "\\enquote{"
        ;; TeX-file-extensions '("Rnw" "rnw" "Snw" "snw" "tex" "sty" "cls" "ltx" "texi" "texinfo" "dtx")
        TeX-parse-self t)


  ;; (add-to-list 'TeX-command-list
  ;;              '("Makeglossaries" "makeglossaries %s" TeX-run-command nil
  ;;                (latex-mode)
  ;;                :help "Run makeglossaries script, which will choose xindy or makeindex") t)

  ;; ;; Font-lock for AuCTeX
  ;; ;; Note: '«' and '»' is by pressing 'C-x 8 <' and 'C-x 8 >', respectively
  ;; (font-lock-add-keywords 'latex-mode (list (list "\\(«\\(.+?\\|\n\\)\\)\\(+?\\)\\(»\\)" '(1 'font-latex-string-face t) '(2 'font-latex-string-face t) '(3 'font-latex-string-face t))))

  ;; Use LaTeX-mode-map to define keybinding doesn't work, even if "save-compile-latex" is defined in LaTeX-mode-map.
  (define-key TeX-mode-map (kbd "<f5>") 'save-compile-latex)
  (define-key TeX-mode-map (kbd "<f7>") 'preview-clearout-buffer)
  (define-key TeX-mode-map (kbd "TAB") 'complete-if-no-space)
  (define-key TeX-mode-map (kbd "<tab>") 'complete-if-no-space)
  
  ;; (load "preview-latex.el" nil t t)
  
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;;  automatically insert  ‘\(...\)’ in LaTeX files by pressing $
(add-hook 'LaTeX-mode-hook
          (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                     (cons "\\(" "\\)"))))

(add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)

(when (maybe-require-package 'company-math)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (setq-local company-backends (add-to-list 'company-backends 'company-math-symbols-latex))
                               (setq-local company-backends (add-to-list 'company-backends 'company-latex-commands))
                               (setq-local company-backends (add-to-list 'company-backends 'company-math-symbols-unicode))))
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local company-backends (add-to-list 'company-backends 'company-math-symbols-unicode))
                             (setq-local company-backends (add-to-list 'company-backends 'company-latex-commands)))))


(when (maybe-require-package 'cdlatex)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-cdlatex-mode)
              (add-hook 'post-self-insert-hook
                        #'(lambda ()
                            (when (looking-back (rx "$ "))
                              (save-excursion
                                (backward-char 1)
                                (org-toggle-latex-fragment))))
                        'append
                        'local))))
;; preview
;; (eval-after-load 'preview
;;   '(progn
;;      (set-default 'preview-scale-function 1.7)
;;      (set-default 'preview-default-option-list
;;                   '("displaymath" "floats" "graphics" "textmath"))))


(provide 'init-latex)