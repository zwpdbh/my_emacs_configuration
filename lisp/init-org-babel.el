;; evaluation lisp using sly instead of slime, need to use org-plus-contrib
(if (featurep 'init-common-lisp-with-slime)
    (setq org-babel-lisp-eval-fn #'slime-eval)
  (setq org-babel-lisp-eval-fn #'sly-eval))

;; http request in org-mode babel, requires curl
(use-package ob-http
  :after (org)
  :defer t
  :ensure t)

(setq zw/org-babel-evaluate-whitelist
      '("emacs-lisp"
        "lisp"
        "scheme"
        "clojure"
        "python"
        "R"
        "C"
        "C++"
        "latex"
        "plantuml"))

(setq zw/org-babel-load-language-list
      '((emacs-lisp . t)
        (lisp . t)
        (clojure . t)
        (scheme . t)
        (C . t)
        (shell . t)
        (js . t)
        (python . t)
        (R . t)
        (http . t)
        (latex . t)
        (plantuml . t)
        (haskell . t)))

(add-hook 'org-mode-hook
          '(lambda ()
             (progn
               ;; all languages needed to be confirmed to execute except:
               (defun my-org-confirm-babel-evaluate (lang body)
                 (not (member lang zw/org-babel-evaluate-whitelist)))
               (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate))))


(after-load 'org
            (defun zw/show-inline-images-after-execute ()
              (when org-inline-image-overlays
                (org-redisplay-inline-images)))
            (add-hook 'org-babel-after-execute-hook 'zw/show-inline-images-after-execute)
            
            ;; since yaml mode is not supported by org, create the command yourself
            (defun org-babel-execute:json (body params) body)
            (defun org-babel-execute:java (body params) body)
            (defun org-babel-execute:cmake (body params) body)
            (defun org-babel-execute:terraform (body params) body)
            (defun org-babel-execute:racket (body params) body)
            (defun org-babel-execute:make (body params) body)
            
            (org-babel-do-load-languages
             'org-babel-load-languages
             zw/org-babel-load-language-list)

            ;; Otherwise, it will has error: duplicated key in org-structure-template-alist
            (setq org-structure-template-alist (remove* "c" org-structure-template-alist :test 'equal :key 'car))
            (add-to-list 'org-structure-template-alist '("c" . "src C"))
            (add-to-list 'org-structure-template-alist '("cpp" . "src C++ :include <stdio.h> :includes <iostream>"))
            
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
            
            (add-to-list 'org-structure-template-alist '("json" . "src json"))
            (add-to-list 'org-structure-template-alist '("java" . "src java"))
            (add-to-list 'org-structure-template-alist '("cmake" . "src cmake"))
            (add-to-list 'org-structure-template-alist '("terraform" . "src terraform"))
            (add-to-list 'org-structure-template-alist '("tex" . "src latex"))
            (add-to-list 'org-structure-template-alist '("text" . "src text"))
            (add-to-list 'org-structure-template-alist '("makefile" . "src makefile")))


;; Do not use flycheck in org-src-mode since it is used most of time for documentation
(add-hook 'org-src-mode-hook
          '(lambda ()
             (flycheck-mode 0)))

(provide 'init-org-babel)
