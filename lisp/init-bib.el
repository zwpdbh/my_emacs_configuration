
;; bib related configuration
(maybe-require-package 'ivy-bibtex)
(defvar my/bib-file-location "~/code/capture-org/bib/library.bib"
  "Where I keep my bib file.")
(setq bibtex-completion-bibliography my/bib-file-location)


;; telling bibtex-completion where your bibliographies can be found
(setq bibtex-completion-bibliography my/bib-file-location)

;; specify the path of the note
(setq bibtex-completion-notes-path "~/Documents/Research/ref.org")
;;If one file per publication is preferred, bibtex-completion-notes-path should point to the directory used for storing the notes files:
;;(setq bibtex-completion-notes-path "/path/to/notes")

;; Customize layout of search results
;; first add journal and booktitle to the search fields
(setq bibtex-completion-additional-search-fields '(journal booktitle))
(setq bibtex-completion-display-formats
      '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${journal:40}")
        (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
        (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
        (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
        (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*}")))

;; Symbols used for indicating the availability of notes and PDF files
(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")

;; default is to open pdf - change that to insert citation
(setq bibtex-completion-pdf-field "File")
;; the pdf should be renamed with the BibTeX key
;;(setq bibtex-completion-library-path '("~/Documents/Literature/"))
;;(setq ivy-bibtex-default-action #'ivy-bibtex-insert-citation)

;; adds an action with Evince as an external viewer bound to P,
;; in addition to the regular Emacs viewer with p
(defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
  (let ((bibtex-completion-pdf-open-function
         (lambda (fpath) (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath))))
    (bibtex-completion-open-pdf keys fallback-action)))
(ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)
(ivy-add-actions
 'ivy-bibtex
 '(("P" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")))

;; If you store files in various formats, then you can specify a list
;; Extensions in this list are then tried sequentially until a file is found.
(setq bibtex-completion-pdf-extension '(".pdf" ".djvu"))

;; format of ciations
;; For example, people who don’t use Ebib might prefer links to the PDFs
;; instead of Ebib-links in org mode files

(setq bibtex-completion-format-citation-functions
      '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
        (latex-mode    . bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (default       . bibtex-completion-format-citation-default)))


;; use org-ref
(require 'org-ref)

(setq org-ref-bibliography-notes "~/Documents/Research/ref.org"
      org-ref-default-bibliography '("~/Documents/Research/library.bib")
      org-ref-pdf-directory "~/Documents/Literature/"
      org-ref-completion-library 'org-ref-ivy-cite
      )
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;; Sometimes it is necessary to tell bibtex what dialect you are using to support the different bibtex entries that are possible in biblatex. You can do it like this globally.
(setq bibtex-dialect 'biblatex)

;; open pdf
(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)


(provide 'init-bib)