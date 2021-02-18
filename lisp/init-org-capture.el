;; * create corresponding .org file within the org folder inside HUGO site
;; * each note/post will be inserted into the corresponding org file under second level headline (the first level is the corresponding file headline)
;; * edit config/menus.toml, create link to section
;; * edit content/home/<corresponding url name>.md, use computer-science.md as example:
;; * line 3: # This section displays recent blog posts from `content/computer-science/`.
;; * title = "Computer Science Posts"
;; * line 15: page_type = "computer-science"
;; * Note: need to mark the second level headline status as DONE to make it be visiable and searchable after being published.


(after-load 'org
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
                   ,(concat "  :EXPORT_OPTIONS: " ":toc:t") ; Table of Contents, see: https://ox-hugo.scripter.co/doc/org-toc/
                   ,(concat "  :EXPORT_HUGO_CUSTOM_FRONT_MATTER+: "  ":weight 10 :autoCollapseToc true :mathjax true :contentCopyright MIT :author \"Z wei\"")
                   "  :END:"
                   "  %?\n")
                 "\n")))

  (defun org-new-article-template ()
    (let* (;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
           (date (format-time-string (org-time-stamp-format  :inactive) (org-current-time)))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   "  :PROPERTIES:"
                   ,(concat "  :EXPORT_FILE_NAME: " fname)
                   ,(concat "  :EXPORT_DATE: " date) 
                   ,(concat "  :EXPORT_OPTIONS: " ":toc:t"))
                 "\n")))

  (defvar org-capture-base "~/code/capture-org/"
    "define the place where we put our org files for hugo")
  (defvar org-capture-todo (concat org-capture-base "todo.org"))
  (defvar org-capture-computer-science (concat org-capture-base "computer-science/" "computer-science.org"))
  (defvar org-capture-emacs (concat org-capture-base "emacs/" "emacs.org"))
  (defvar org-capture-math (concat org-capture-base "mathematics/" "mathematics.org"))
  (defvar org-capture-software (concat org-capture-base "software-engineering/" "software-engineering.org"))
  (defvar org-capture-tools (concat org-capture-base "tools/" "tools.org"))

  (setq org-capture-templates
        '(("h" "Hugo post")
          
          ("t" "todo"
           entry (file org-capture-todo)
           "* TODO %? :TODO: \n Added:%T\n"
           :clock-in t :clock-resume t)

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
           :clock-in t :clock-resume t))))

(provide 'init-org-capture)