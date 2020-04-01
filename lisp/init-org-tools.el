;; === Github Flavored Markdown
(use-package ox-gfm
  :defer t
  :ensure t
  :config
  (progn
    (eval-after-load "org"
      '(require 'ox-gfm nil t))))

;; === Capture screenshot within Emacs
(use-package org-attach-screenshot
  :commands (org-mode)
  :ensure t
  :config
  (progn
    (setq org-attach-screenshot-dirfunction
          (lambda () 
            (progn (assert (buffer-file-name))
                   (concat (file-name-sans-extension (buffer-file-name))
                           "_att")))
          org-attach-screenshot-relative-links t)))

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

;; (global-set-key "\C-cs" 'zw/org-screenshot)
(add-hook 'org-mode-hook '(lambda ()
                            (if *win64*
                                (define-key org-mode-map (kbd "\C-c s") 'zw/org-screenshot)
                              (define-key org-mode-map (kbd "\C-c s") 'org-attach-screenshot))))

;; === Org-download moving images from A to B
(use-package org-download
  :commands (org-mode)
  :ensure t
  :config
  (progn
    (add-hook 'dired-mode-hook 'org-download-enable)))

(provide 'init-org-tools)