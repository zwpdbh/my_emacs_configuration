;; === Capture screenshot within Emacs
;; By default, it utilizes the "import" tool available in the ImageMagick package: sudo apt install imagemagick
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


;; Up-to-date table of contents in org files under a heading with tag: TOC
;; So, create "Table of content" heading, then add it with tag: TOC.
;; Now, anytime, you save the org file, it will generate/update table-of-content under that heading.
(when (maybe-require-package 'toc-org)
  (add-hook 'org-mode-hook 'toc-org-mode)
  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  (after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point))

  (after-load 'org
    (add-to-list 'org-tag-alist '("TOC" . ?T))))

;; Add additional tags shortcut when adding tags through C-c C-q.
;; See: https://orgmode.org/manual/Setting-Tags.html#Setting-tags
(after-load 'org
  (add-to-list 'org-tag-alist '("todo" . ?t))
  (add-to-list 'org-tag-alist '("algorithm" . ?a))
  (add-to-list 'org-tag-alist '("elisp" . ?e))
  (add-to-list 'org-tag-alist '("common-lisp" . ?c)))


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


;; By John Kitchin:
;; https://emacs.stackexchange.com/questions/29385/generate-a-toc-index-org-file-out-our-headings-from-all-org-files
(defun zw/org-toc ()
  "List all headings in your org file from current folder recursively."
  (interactive)
  (let ((files (f-entries "." (lambda (f) (f-ext? f "org")) t))
        (headlines '())
        choice) 
    (loop for file in files do
          (with-temp-buffer 
            (insert-file-contents file) 
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (cl-pushnew (list
                           (format "%-80s (%s)"
                                   (match-string 0)
                                   (file-name-nondirectory file))
                           :file file
                           :position (match-beginning 0))
                          headlines))))
    (setq choice
          (completing-read "Headline: " (reverse headlines)))
    (find-file (plist-get (cdr (assoc choice headlines)) :file))
    (goto-char (plist-get (cdr (assoc choice headlines)) :position))))


;; How to copy links OUT of org-mode?
;; https://emacs.stackexchange.com/questions/3981/how-to-copy-links-out-of-org-mode
(defun zw/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(after-load 'org
            (define-key org-mode-map (kbd "C-x C-l") 'zw/org-link-copy))



(provide 'init-org-tools)
