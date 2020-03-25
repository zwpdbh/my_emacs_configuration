;; * create corresponding .org file within the org folder inside HUGO site
;; * each note/post will be inserted into the corresponding org file under second level headline (the first level is the corresponding file headline)
;; * edit config/menus.toml, create link to section
;; * edit content/home/<corresponding url name>.md, use computer-science.md as example:
;; * line 3: # This section displays recent blog posts from `content/computer-science/`.
;; * title = "Computer Science Posts"
;; * line 15: page_type = "computer-science"
;; * Note: need to mark the second level headline status as DONE to make it be visiable and searchable after being published.

(use-package ox-hugo
  :ensure t)

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
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_FILE_NAME: " fname)
                 ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                 ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: "  ":weight 10 :autoCollapseToc true :mathjax true :contentCopyright MIT :author \"Z wei\"")
                 ":END:"
                 "%?\n")          ;Place th
               "\n")))

(defvar hugo-org-path "~/code/capture-org/"
  "define the place where we put our org files for hugo")
(defvar org-capture-todo (concat hugo-org-path "todo.org"))
(defvar org-capture-computer-science (concat hugo-org-path "computer-science.org"))
(defvar org-capture-emacs (concat hugo-org-path "emacs.org"))
(defvar org-capture-math (concat hugo-org-path "mathematics.org"))
(defvar org-capture-software (concat hugo-org-path "software-engineering.org"))
(defvar org-capture-tools (concat hugo-org-path "tools.org"))
(defvar org-capture-work (concat hugo-org-path "work-notes.org"))
(defvar org-capture-test (concat hugo-org-path "test.org"))
;; (defvar hugo-capture-orgs
;;   (list
;;    (cons 'computer (concat hugo-org-path "computer-science.org"))
;;    (cons 'emacs (concat hugo-org-path "emacs.org"))
;;    (cons 'math (concat hugo-org-path "mathematics.org"))
;;    (cons 'software (concat hugo-org-path "software-engineering.org"))
;;    (cons 'tools (concat hugo-org-path "tools.org"))
;;    (cons 'work (concat hugo-org-path "work-notes.org"))))

(setq org-export-with-author nil)
(setq org-capture-templates
      '(
        ("t" "todo" entry (file org-capture-todo)
         "* TODO %? :TODO: \n Added:%T\n"
         :clock-in t :clock-resume t)

        ;; ("ht" "test" entry (file org-capture-test)
        ;;  (function org-hugo-new-subtree-post-capture-template)
        ;;  :clock-in t :clock-resume t)

        ("h" "Hugo post")

        ;; ("hc" "Computer-Science"
        ;;  entry (file+olp org-capture-computer-science "Computer-Science")
        ;;  (function org-hugo-new-subtree-post-capture-template)
        ;;  :clock-in t :clock-resume t)
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
         ;; entry (file+olp org-capture-tools "Tools")
         entry (file org-capture-tools)
         (function org-hugo-new-subtree-post-capture-template)
         :clock-in t :clock-resume t)

        ("hw" "Work-Notes"
         entry (file org-capture-work)
         (function org-hugo-new-subtree-post-capture-template)
         :clock-in t :clock-resume t)))

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