;; sudo apt install pandoc
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(when (maybe-require-package 'markdown-toc)
  ;; make markdown-toc only generated toc with maximum level = 2
  (custom-set-variables '(markdown-toc-user-toc-structure-manipulation-fn
			  (lambda (toc-structure)
			    (-filter (lambda (l) (let ((index (car l)))
					      (> 2 index)))
				     toc-structure))))
  
  ;; In markdown file, run "M-x markdown-toc-generate-toc"
  (add-hook 'markdown-mode-hook 'markdown-toc-mode))


(provide 'init-markdown)
