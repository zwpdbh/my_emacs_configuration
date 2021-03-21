;; === Capture screenshot within Emacs
(use-package org-attach-screenshot
  :commands (org-mode)
  :ensure t
  :config
  (progn    
    (setq org-attach-screenshot-dirfunction
          '(lambda () 
             (progn (assert (buffer-file-name))
                    (concat (file-name-sans-extension (buffer-file-name))
                            "_att")))
          org-attach-screenshot-relative-links t)))

;; create org-screenshot function based on system
(if *win64*
    (defun zw/org-screenshot ()
      "Take a screenshot into a time stamped unique-named file in the
     same directory as the org-buffer and insert a link to this file."
      (interactive)
      (setq filename
            (concat
             (make-temp-name
              (concat (file-name-directory buffer-file-name)
                      "_"
                      (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
      (shell-command "snippingtool /clip")
      (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
      (insert (concat "[[file:" filename "]]"))
      (org-display-inline-images))
  (defalias 'zw/org-screenshot 'org-attach-screenshot))

(add-hook 'org-mode-hook '(lambda ()
                            (define-key org-mode-map (kbd "\C-c s") 'zw/org-screenshot)))

;; === Org-download moving images from A to B
(use-package org-download
             :commands (org-mode)
             :ensure t
             :config
             (progn
               (add-hook 'dired-mode-hook 'org-download-enable)))

;; Github Flavored Markdown exporter for Org Mode
(when (maybe-require-package 'ox-gfm)
  (after-load 'org
              (require 'ox-gfm nil t)))

;;export content of subtrees without their headings
(after-load 'ox-extra
  (ox-extras-activate '(ignore-headlines)))

;; ;; prerequisite: install pandoc on system
;; (when (maybe-require-package 'ox-pandoc)
;;   (setq org-pandoc-menu-entry '(
;;                                 (77 "to markdown." org-pandoc-export-to-markdown)
;;                                 (109 "to markdown and open." org-pandoc-export-to-markdown-and-open)
;;                                 (88 "to docx." org-pandoc-export-to-docx)
;;                                 (120 "to docx and open." org-pandoc-export-to-docx-and-open)))

;;   ;; TODO figure out what exactly these 2 options mean 
;;   (setq org-pandoc-options '((standalone . nil)))
  
;;   ;; create custom reference file for style using
;;   ;; pandoc --print-default-data-file reference.docx > custom-reference.docx
;;   (setq org-pandoc-options-for-docx '(
;;                                       ;; (toc . t)
;;                                       ;; (toc-depth . 2)
;;                                       (reference-doc . "~/.emacs.d/pandoc-templates/custom-reference.docx")))
;;   (setq org-pandoc-options-for-markdown '(
;;                                           (toc . t)
;;                                           (toc-depth . 2)))
;;   (after-load 'ox
;;     (require 'ox-pandoc)))


(when (maybe-require-package 'toc-org)
  (add-hook 'org-mode-hook 'toc-org-mode)

  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode)

  (after-load 'org
    ;; Add additional tags shortcut when adding tags through C-c C-q.
    (add-to-list 'org-tag-alist '("Lisp" . ?l))
    (add-to-list 'org-tag-alist '("Emacs" . ?e))
    (add-to-list 'org-tag-alist '("TODO" . ?t))
    (add-to-list 'org-tag-alist '("Go" . ?g))))


;; ;; Set the background of org-exported <code> blocks according to theme
;; (defun my/org-inline-css-hook (exporter)
;;   "Insert custom inline css to automatically set the
;;      background of code to whatever theme I'm using's background"
;;   (when (eq exporter 'html)
;;     (let* ((my-pre-bg (face-background 'default))
;; 	       (my-pre-fg (face-foreground 'default)))
;;       (setq
;;        org-html-head-extra
;;        (concat
;; 	    org-html-head-extra
;; 	    (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
;; 		        my-pre-bg my-pre-fg))))))
;; (add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

(provide 'init-org-tools)
