(use-package htmlize
  :defer t
  :commands (org-export-dispatch)
  :ensure t)

;; publish the ~/code/org/ project to HTML
(eval-after-load 'org
  #'(lambda ()
      (require 'ox-publish)
      (setq org-publish-project-alist
            '(;; the netes components, it publishes all the org-mode files to HTML 
	      ("org-notes"
	       :base-directory "~/code/org/"
	       :base-extension "org"
	       :publishing-directory "~/code/public_html/"
	       :recursive t
	       :publishing-function org-html-publish-to-html
	       :headline-levels 4
	       :auto-preamble t
	       :auto-sitemap t                  
	       :sitemap-filename "sitemap.org"  
	       :sitemap-title "Sitemap")
	      ("org-static"
	       :base-directory "~/code/org/"
	       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	       :publishing-directory "~/public_html/"
	       :recursive t
	       :publishing-function org-publish-attachment)
	      ("org"
               :components ("org-notes" "org-static"))
	      ("hugo-notes"
	       :base-directory "~/code/org/"
	       :base-extension "org"
	       :publishing-directory "~/code/my-site/content-org/"
	       :recursive t)))))

(provide 'init-org-html)