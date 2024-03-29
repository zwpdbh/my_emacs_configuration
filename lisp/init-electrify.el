;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit nil)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; electric return in parenthesis
(defvar electrify-return-match
  "[\]}\)\"\']"
  "If this regexp matches the text after the cursor, do an \"electric\"
        return.")

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

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
        open and indent an empty line between the cursor and the text.  Move the
        cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (progn
          (save-excursion
            ;; This is where the other part of match got placed
            (newline-and-indent))
          ;; This is where the cursor got placed
          (newline-and-previous-indent)
          (insert-tab))
      (progn
        (newline arg)
        (indent-according-to-mode)))))

(defun zw/electrify-return-for-lisp (arg)
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (progn
          (save-excursion
           (newline-and-indent))
          (newline-and-indent))
        (progn
          (newline-and-indent)))))

(defun zw/electrify-return-for-erlang (arg)
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (progn
          (save-excursion
            (newline-and-indent))
          (newline-and-previous-indent)
          (insert-tab))
      (progn
        (newline-and-indent)))))

(defun zw/electrify-return-for-elixir (arg)
  (interactive "P")
  (progn
    ;; (save-excursion
    ;;   (newline-and-indent))
    (newline-and-indent)))


(defun zw/set-electrify-return ()
  (interactive)
  (define-key (current-local-map) (kbd "RET") 'electrify-return-if-match))

(defun zw/unset-electrify-return ()
  (interactive)
  (local-unset-key (kbd "RET"))
  (define-key (current-local-map) (kbd "RET") 'newline-and-indent))

(defun zw/set-electrify-return-for-lisp ()
  (interactive)
  (local-unset-key (kbd "RET"))
  (define-key (current-local-map) (kbd "RET") 'zw/electrify-return-for-lisp))

(defun zw/set-electrify-return-for-erlang ()
  (interactive)
  (local-unset-key (kbd "RET"))
  (define-key (current-local-map) (kbd "RET") 'zw/electrify-return-for-erlang))

(defun zw/set-electrify-return-for-elixir ()
  (interactive)
  (local-unset-key (kbd "RET"))
  (setq-local backward-delete-char-untabify-method 'untabify)
  (setq-local electric-indent-inhibit nil)
  (define-key elixir-mode-map (kbd "RET") 'zw/electrify-return-for-elixir))

(add-hook 'prog-mode-hook 'zw/set-electrify-return)
(add-hook 'conf-mode-hook 'zw/set-electrify-return)
(add-hook 'text-mode-hook 'zw/set-electrify-return)

(add-hook 'c-mode-hook 'zw/unset-electrify-return)
(add-hook 'c++-mode-hook 'zw/unset-electrify-return)
(add-hook 'emacs-lisp-mode-hook #'zw/unset-electrify-return)

(add-hook 'lisp-mode-hook #'zw/set-electrify-return-for-lisp)
(add-hook 'tuareg-mode-hook #'zw/set-electrify-return-for-lisp)

(add-hook 'erlang-mode-hook #'zw/set-electrify-return-for-erlang)
(add-hook 'elixir-mode-hook #'zw/set-electrify-return-for-elixir)

;; Auto-indent yanked (pasted) code
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode
                                     lisp-mode
                                     clojure-mode
                                     scheme-mode
                                     haskell-mode
                                     python-mode
                                     c-mode
                                     c++-mode
                                     objc-mode
                                     latex-mode
                                     tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(provide 'init-electrify)