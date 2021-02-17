(use-package htmlize
  :defer t
  :commands (org-export-dispatch)
  :ensure t)

(use-package ox-reveal
  :ensure t
  :commands (org-export-dispatch)
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (setq org-reveal-mathjax t))

;; ref: https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html#org625a5c5
(after-load 'org
  (require 'ox-publish)
  ;; (setq org-publish-project-alist
  ;;       '(;; the netes components, it publishes all the org-mode files to HTML 
  ;;         ("org-notes"
  ;;          :base-directory "~/code/capture-org/"
  ;;          :base-extension "org"
  ;;          :publishing-directory "~/code/org-site/"
  ;;          :recursive t
  ;;          :publishing-function org-html-publish-to-html
  ;;          :auto-sitemap t)
  ;;         ("org-static"
  ;;          :base-directory "~/code/capture-org/"
  ;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
  ;;          :publishing-directory "~/code/org-site/"
  ;;          :recursive t
  ;;          :publishing-function org-publish-attachment)
  ;;         ("org"
  ;;          :components ("org-notes" "org-static"))))

  ;; setting to nil, avoids "Author: x" at the bottom
  (setq org-export-with-section-numbers nil
        org-export-with-smart-quotes t
        org-export-with-toc nil)

  (defvar this-date-format "%b %d, %Y")

  (setq org-html-divs '((preamble "header" "top")
                        (content "main" "content")
                        (postamble "footer" "postamble"))
        org-html-container-element "section"
        org-html-metadata-timestamp-format this-date-format
        org-html-checkbox-type 'html
        org-html-html5-fancy t
        org-html-validation-link t
        org-html-doctype "html5"
        org-html-htmlize-output-type 'css
        org-src-fontify-natively t)


  (defvar me/website-html-head
    "<link rel='icon' type='image/x-icon' href='/images/favicon.ico'/>
<meta name='viewport' content='width=device-width, initial-scale=1'>
<link rel='stylesheet' href='https://code.cdn.mozilla.net/fonts/fira.css'>
<link rel='stylesheet' href='/css/site.css?v=2' type='text/css'/>
<link rel='stylesheet' href='/css/custom.css' type='text/css'/>
<link rel='stylesheet' href='/css/syntax-coloring.css' type='text/css'/>")

  (defun me/website-html-preamble (plist)
    "PLIST: An entry."
    (if (org-export-get-date plist this-date-format)
        (plist-put plist
                   :subtitle (format "Published on %s by %s."
                                     (org-export-get-date plist this-date-format)
                                     (car (plist-get plist :author)))))
    ;; Preamble
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/html-templates/preamble.html") (buffer-string)))

  (defun me/website-html-postamble (plist)
    "PLIST."
    (concat (format
             (with-temp-buffer
               (insert-file-contents "~/.emacs.d/html-templates/postamble.html") (buffer-string))
             (format-time-string this-date-format (plist-get plist :time)) (plist-get plist :creator))))

  (defvar site-attachments
    (regexp-opt '("jpg" "jpeg" "gif" "png" "svg"
                  "ico" "cur" "css" "js" "woff" "html" "pdf"))
    "File types that are published as static files.")


  (defun me/org-sitemap-format-entry (entry style project)
    "Format posts with author and published data in the index page.

ENTRY: file-name
STYLE:
PROJECT: `posts in this case."
    (cond ((not (directory-name-p entry))
           (format "*[[file:%s][%s]]*
                 #+HTML: <p class='pubdate'>by %s on %s.</p>"
                   entry
                   (org-publish-find-title entry project)
                   (car (org-publish-find-property entry :author project))
                   (format-time-string this-date-format
                                       (org-publish-find-date entry project))))
          ((eq style 'tree) (file-name-nondirectory (directory-file-name entry)))
          (t entry)))


  (defun me/org-reveal-publish-to-html (plist filename pub-dir)
    "Publish an org file to reveal.js HTML Presentation.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory. Returns output file name."
    (let ((org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))
      (org-publish-org-to 'reveal filename ".html" plist pub-dir)))

  (setq org-publish-project-alist
        `(("posts"
           :base-directory "~/code/capture-org/"
           :base-extension "org"
           :recursive t
           :publishing-function org-html-publish-to-html
           :publishing-directory "~/code/org-site/"
           :exclude ,(regexp-opt '("README.org" "draft"))
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Blog Index"
           :sitemap-format-entry me/org-sitemap-format-entry
           :sitemap-style list
           :sitemap-sort-files anti-chronologically
           :html-link-home "/"
           :html-link-up "/"
           :html-head-include-scripts t
           :html-head-include-default-style nil
           :html-head ,me/website-html-head
           :html-preamble me/website-html-preamble
           :html-postamble me/website-html-postamble)
          ("css"
           :base-directory "~/code/capture-org/css"
           :base-extension "css"
           :publishing-directory "~/code/org-site/css"
           :publishing-function org-publish-attachment
           :recursive t)
          ("images"
           :base-directory "~/code/capture-org/img"
           :base-extension ,site-attachments
           :publishing-directory "~/code/org-site/img"
           :publishing-function org-publish-attachment
           :recursive t)
          ("assets"
           :base-directory "~/code/capture-org/assets"
           :base-extension ,site-attachments
           :publishing-directory "~/code/org-site/assets"
           :publishing-function org-publish-attachment
           :recursive t)
          ("all" :components ("posts" "css" "images" "assets")))))

(provide 'init-org-html)