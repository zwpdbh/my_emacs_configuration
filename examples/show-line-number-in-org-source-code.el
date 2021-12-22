;; https://kitchingroup.cheme.cmu.edu/blog/2015/10/13/Line-numbers-in-org-mode-code-blocks/
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

(defvar number-line-overlays '()
  "List of overlays for line numbers.")

(make-variable-buffer-local 'number-line-overlays)

(defun number-line-src-block ()
  (interactive)
  (save-excursion
   (let* ((src-block (org-element-context))
          (nlines (- (length
                      (s-split
                       "\n"
                       (org-element-property :value src-block)))
                     1)))
     (goto-char (org-element-property :begin src-block))
     (re-search-forward (regexp-quote (org-element-property :value src-block)))
     (goto-char (match-beginning 0))

     (loop for i from 1 to nlines
           do
              (beginning-of-line)
              (let (ov)
                (setq ov (make-overlay (point) (point)))
                (overlay-put ov 'before-string (format "%3s" (number-to-string i)))
                (add-to-list 'number-line-overlays ov))
              (next-line))))

  ;; now read a char to clear them
  (read-key "Press a key to clear numbers.")
  (mapc 'delete-overlay number-line-overlays)
  (setq number-line-overlays '()))

;; While inside code-block, M-x number-line-src-block
(number-line-src-block)
