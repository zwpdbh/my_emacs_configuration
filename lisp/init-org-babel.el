;; evaluation lisp using sly instead of slime, need to use org-plus-contrib
(setq org-babel-lisp-eval-fn #'sly-eval)

;; http request in org-mode babel, requires curl
(use-package ob-http
  :after (org)
  :defer t
  :ensure t)

;;;; https://github.com/DEADB17/ob-racket
;; (use-package ob-racket
;;   :after org
;;   :defer t
;;   :ensure t)

(add-hook 'org-mode-hook #'(lambda ()
                             (progn
                               ;; all languages needed to be confirmed to execute except:
                               (defun my-org-confirm-babel-evaluate (lang body)
                                (not (member lang '("emacs-lisp" "lisp" "scheme" "clojure" "python" "R" "C" "latex" "dot" "plantuml"))))
                               (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate))))

(eval-after-load 'org
  #'(lambda ()
      ;; since yaml mode is not supported by org, create the command yourself
      (defun org-babel-execute:yaml (body params) body)
      (defun org-babel-execute:json (body params) body)
      (defun org-babel-execute:example (body params) body)
      (defun org-babel-execute:terraform (body params) body)
      (defun org-babel-execute:racket (body params) body)
      
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (lisp . t)
         (clojure . t)
         (scheme . t)
         ;; (racket . t)
         (C . t)
         (shell . t)
         (js . t)
         (python . t)
         (R . t)
         (http . t)
         (latex . t)
         (dot . t)
         (plantuml . t)))

      ;; (add-to-list 'org-structure-template-alist '("racket" . "src racket :lang sicp"))
      (add-to-list 'org-structure-template-alist '("py3" . "src python3"))
      (add-to-list 'org-structure-template-alist '("py" . "src python"))
      (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
      (add-to-list 'org-structure-template-alist '("lisp" . "src lisp"))
      (add-to-list 'org-structure-template-alist '("scheme" . "src scheme"))
      (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
      (add-to-list 'org-structure-template-alist '("clojure" . "src clojure"))
      (add-to-list 'org-structure-template-alist '("r" . "src R"))
      (add-to-list 'org-structure-template-alist '("js" . "src js"))
      (add-to-list 'org-structure-template-alist '("http" . "src http"))
      (add-to-list 'org-structure-template-alist '("lt" . "LaTeX"))
      (if *win64*
          (add-to-list 'org-structure-template-alist '("dot" . "src dot :cmdline -Kdot -Tpng :file img/tmp.png"))
        (add-to-list 'org-structure-template-alist '("dot" . "src dot :file img/tmp.png")))
      ;; set the major-mode for edit babel dot src block 
      (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
      (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
      (add-to-list 'org-structure-template-alist '("json" . "src json"))
      (add-to-list 'org-structure-template-alist '("ex" . "example"))
      (add-to-list 'org-structure-template-alist '("terraform" . "src terraform"))
      (add-to-list 'org-structure-template-alist '("uml" . "src plantuml"))
      (add-to-list 'org-structure-template-alist '("latex" . "src latex"))))

(provide 'init-org-babel)