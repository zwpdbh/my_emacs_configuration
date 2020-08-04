;; * create corresponding .org file within the org folder inside HUGO site
;; * each note/post will be inserted into the corresponding org file under second level headline (the first level is the corresponding file headline)
;; * edit config/menus.toml, create link to section
;; * edit content/home/<corresponding url name>.md, use computer-science.md as example:
;; * line 3: # This section displays recent blog posts from `content/computer-science/`.
;; * title = "Computer Science Posts"
;; * line 15: page_type = "computer-science"
;; * Note: need to mark the second level headline status as DONE to make it be visiable and searchable after being published.


(when (maybe-require-package 'ox-hugo)
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* (;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
           (date (format-time-string (org-time-stamp-format  :inactive) (org-current-time)))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   "  :PROPERTIES:"
                   ,(concat "  :EXPORT_FILE_NAME: " fname)
                   ,(concat "  :EXPORT_DATE: " date) ;Enter current date and time
                   ,(concat "  :EXPORT_OPTIONS: " ":toc:t") ;TOC
                   ,(concat "  :EXPORT_HUGO_CUSTOM_FRONT_MATTER+: "  ":weight 10 :autoCollapseToc true :mathjax true :contentCopyright MIT :author \"Z wei\"")
                   "  :END:"
                   "  %?\n")          ;Place th
                 "\n")))

  (defvar hugo-org-path "~/code/capture-org/"
    "define the place where we put our org files for hugo")
  (defvar org-capture-todo (concat hugo-org-path "todo.org"))
  (defvar org-capture-computer-science (concat hugo-org-path "computer-science/" "computer-science.org"))
  (defvar org-capture-emacs (concat hugo-org-path "emacs/" "emacs.org"))
  (defvar org-capture-math (concat hugo-org-path "mathematics/" "mathematics.org"))
  (defvar org-capture-software (concat hugo-org-path "software-engineering/" "software-engineering.org"))
  (defvar org-capture-tools (concat hugo-org-path "tools/" "tools.org"))

  (setq org-export-with-author nil)
  (setq org-hugo-export-with-toc 2)
  (setq org-capture-templates
        '(("h" "Hugo post")
          
          ("t" "todo"
           entry (file org-capture-todo)
           "* TODO %? :TODO: \n Added:%T\n"
           :clock-in t :clock-resume t)

          ;; ;; This will make the captured note appear as a second level heading under
          ;; ;; the first level heading Computer-Science
          ;; ("hc" "Computer-Science"
          ;;  entry (file+olp org-capture-computer-science "Computer-Science")
          ;;  (function org-hugo-new-subtree-post-capture-template)
          ;;  :clock-in t :clock-resume t)

          ;; This will make the captured note appear as the first level heading in the computer-science.org
          ("hc" "Computer-Science"
           entry (file org-capture-computer-science)
           (function org-hugo-new-subtree-post-capture-template)
           :clock-in t :clock-resume t)

          ("he" "Emacs"
           entry (file org-capture-emacs)
           (function org-hugo-new-subtree-post-capture-template)
           :clock-in t :clock-resume t)

          ("hm" "Mathematics"
           entry (file org-capture-math)
           (function org-hugo-new-subtree-post-capture-template)
           :clock-in t :clock-resume t)

          ("hs" "Software-Engineering"
           entry (file org-capture-software)
           (function org-hugo-new-subtree-post-capture-template)
           :clock-in t :clock-resume t)

          ("ht" "Tools"
           entry (file org-capture-tools)
           ;; entry (file org-capture-tools)
           (function org-hugo-new-subtree-post-capture-template)
           :clock-in t :clock-resume t)))
  (require 'ox-hugo))

;; Set the background of org-exported <code> blocks according to theme
(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
     background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
	   (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
	org-html-head-extra
	(format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
		my-pre-bg my-pre-fg))))))
(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

(provide 'init-org-blog)