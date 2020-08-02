;; refs
;; The TeX related components explaination: http://www.tug.org/levels.html
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-LaTeX.html
;; http://www.stat.rice.edu/~helpdesk/compguide/node39.html

;;; ZW/Note
;; On windows10
;; Install MiKTex or texlive 
;; Add its exe into system-path: "C:/tools/MiKTeX/miktex/bin/x64"
;; If met error when F5, which is save-compile-latex; run MiKTex console and updates its packages

;; On Ubuntu
;; sudo apt install auctex
;; sudo apt-get install latexmk
;; sudo apt install texlive-xetex


(when (maybe-require-package 'auctex)
  (cond ((eq system-type 'darwin)
         (setq exec-path (append exec-path '("/Library/TeX/texbin/")))))
  (when (maybe-require-package 'auctex-latexmk)
    (auctex-latexmk-setup)))


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


(when (maybe-require-package 'latex-preview-pane)
  (setq preview-orientation 'above)
  (setq doc-view-shrink-factor 1.5)
  (add-hook 'LaTeX-mode-hook
            #'(lambda ()
                (latex-preview-pane-enable)
                (latex-preview-pane-mode t))))

(if (executable-find "xelatex")
    (setq org-latex-compiler "xelatex")
  (setq org-latex-compiler "pdflatex"))


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

  (setq TeX-auto-save t
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
        TeX-parse-self t)

  ;; Use LaTeX-mode-map to define keybinding doesn't work, even if "save-compile-latex" is defined in LaTeX-mode-map.
  (define-key TeX-mode-map (kbd "<f5>") 'save-compile-latex)
  (define-key TeX-mode-map (kbd "<f7>") 'preview-clearout-buffer)
  (define-key TeX-mode-map (kbd "TAB") 'complete-if-no-space)
  (define-key TeX-mode-map (kbd "<tab>") 'complete-if-no-space)
  
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;;  automatically insert  ‘\(...\)’ in LaTeX files by pressing $
(add-hook 'LaTeX-mode-hook
          (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                     (cons "\\(" "\\)"))))

(add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook #'linum-mode)

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


(provide 'init-latex)
