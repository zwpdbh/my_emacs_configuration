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
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

;;export content of subtrees without their headings
(after-load 'ox-extra
  (ox-extras-activate '(ignore-headlines)))

(when (maybe-require-package 'ox-pandoc)
  (setq org-pandoc-menu-entry '(
                                (88 "to docx." org-pandoc-export-to-docx)
                                (120 "to docx and open." org-pandoc-export-to-docx-and-open)))

  ;; TODO figure out what exactly these 2 options mean 
  (setq org-pandoc-options '((standalone . nil)))
  
  ;; create custom reference file for style using
  ;; pandoc --print-default-data-file reference.docx > custom-reference.docx
  (setq org-pandoc-options-for-docx '(
                                      (toc . t)
                                      (toc-depth . 2)
                                      (reference-doc . "~/.emacs.d/pandoc-templates/custom-reference.docx")))
  
  (after-load 'ox
    (require 'ox-pandoc)))

(provide 'init-org-tools)