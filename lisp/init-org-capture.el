(after-load 'org
  (defun org-new-article-template ()
    ;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
    (let* ((date (format-time-string (org-time-stamp-format  :inactive) (org-current-time)))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   "  :PROPERTIES:"
                   ,(concat "  :EXPORT_FILE_NAME: " fname)
                   ,(concat "  :EXPORT_DATE: " date) 
                   ,(concat "  :EXPORT_OPTIONS: " ":toc:3")
                   ,(concat "  :EXPORT_OPTIONS: " ":mathjax true")
                   ,(concat "  :EXPORT_OPTIONS: " ":contentCopyright MIT"))
                 "\n")))

  (when (file-directory-p "~/code/capture-org/")
    (defvar org-capture-base "~/code/capture-org/"
      "define the place where we put our org files for hugo")
    
    (defvar org-capture-todo (concat org-capture-base "todo.org"))
    (defvar org-capture-computer-science (concat org-capture-base "computer-science/" "computer-science.org"))
    (defvar org-capture-emacs (concat org-capture-base "emacs/" "emacs.org"))
    (defvar org-capture-math (concat org-capture-base "mathematics/" "mathematics.org"))
    (defvar org-capture-software (concat org-capture-base "software-engineering/" "software-engineering.org"))
    (defvar org-capture-tools (concat org-capture-base "tools/" "tools.org"))

    (setq org-capture-templates
          '(("p" "post")
            
            ("t" "todo"
             entry (file org-capture-todo)
             "* TODO %? :TODO: \n Added:%T\n"
             :clock-in t :clock-resume t)

            ;; This will make the captured note appear as the first level heading in the computer-science.org
            ("pc" "Computer-Science"
             entry (file org-capture-computer-science)
             (function org-new-article-template)
             :clock-in t :clock-resume t)

            ("pe" "Emacs"
             entry (file org-capture-emacs)
             (function org-new-article-template)
             :clock-in t :clock-resume t)

            ("pm" "Mathematics"
             entry (file org-capture-math)
             (function org-new-article-template)
             :clock-in t :clock-resume t)

            ("ps" "Software-Engineering"
             entry (file org-capture-software)
             (function org-new-article-template)
             :clock-in t :clock-resume t)

            ("pt" "Tools"
             entry (file org-capture-tools)
             ;; entry (file org-capture-tools)
             (function org-new-article-template)
             :clock-in t :clock-resume t)))))

(provide 'init-org-capture)
