(defun newline-and-previous-indent ()
	"Insert a newline character and indent to the previous line.

	If the left-behind line is all whitespace, trim it to \\n."
	(interactive)
	(when (region-active-p)
		(delete-region (region-beginning) (region-end)))
	(let ((p (point)) (indent ""))
		(save-excursion
		 (beginning-of-line)
		 (when (or (not (equal (point) p)) t)
			 (if nil
					 ;; skip c-style comments, too. TODO: mode-specific
					 (re-search-forward "\\(//\\)?[ \t]*")
					 (re-search-forward "[ \t]*"))
			 (setq indent (concat indent (match-string 0)))
			 (when (looking-at "$") ; whitespace only line? clear it so we don't leave trailing whitespace
				 (delete-horizontal-space))))
		(insert "\n")
		(delete-horizontal-space)
		(insert indent)))