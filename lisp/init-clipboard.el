;; -*- coding: utf-8; lexical-binding: t; -*-

;; Use the system clipboard
;; @see https://www.emacswiki.org/emacs/CopyAndPaste
;; So `C-y' could paste from clipbord if you are NOT using emacs-nox
;; I only use `paste-from-x-clipboard', not `C-y'.

;; Require ~/.emacs.d/site-lisp/xclip/xclip.el when Emacs is running in terminal
(unless (display-graphic-p)
  ;; This two are must set
  (autoload 'xclip-set-selection "xclip" "" t)
  (autoload 'xclip-get-selection "xclip" "" t)
  
  (add-hook 'after-init-hook #'(lambda ()
                               (load-file "~/.emacs.d/site-lisp/xclip/xclip.el"))))


(setq x-select-enable-clipboard t
      x-select-enable-primary t)

;; kill-ring and clipboard are same? No, it's annoying!
(setq save-interprogram-paste-before-kill nil)

(defun copy-yank-str (msg &optional clipboard-only)
  (unless clipboard-only (kill-new msg))
  (my-pclip msg)
  msg)

(defun cp-filename-of-current-buffer (&optional n)
  "Copy file name (NOT full path) into the yank ring and OS clipboard.
If N is not nil, copy file name and line number."
  (interactive "P")
  (when buffer-file-name
    (let* ((filename (file-name-nondirectory buffer-file-name))
           (s (if n (format "%s:%s" filename (line-number-at-pos)) filename)))
      (copy-yank-str s)
      (message "%s => clipboard&kill-ring" s))))

;; (defun cp-ffip-ivy-last ()
;;   "Copy visible keys of `ivy-last' into `kill-ring' and clipboard."
;;   (interactive)
;;   (my-ensure 'find-file-in-project)
;;   (when ffip-ivy-last-saved
;;     (copy-yank-str
;;      (mapconcat (lambda (e)
;;                   (format "%S" (if (consp e) (car e) e)))
;;                 (ivy-state-collection ffip-ivy-last-saved)
;;                 "\n"))
;;     (message "%d items from ivy-last => clipboard & yank ring" (length ivy-last))))

;; (defun cp-fullpath-of-current-buffer ()
;;   "Copy full path into the yank ring and OS clipboard"
;;   (interactive)
;;   (when buffer-file-name
;;     (copy-yank-str (file-truename buffer-file-name))
;;     (message "file full path => clipboard & yank ring")))

;; (defun clipboard-to-kill-ring ()
;;   "Copy from clipboard to `kill-ring'."
;;   (interactive)
;;   (let* ((warning-minimum-level :emergency))
;;     (kill-new (my-gclip)))
;;   (message "clipboard => kill-ring"))

;; (defun kill-ring-to-clipboard ()
;;   "Copy from `kill-ring' to clipboard."
;;   (interactive)
;;   (my-select-from-kill-ring (lambda (s)
;;                               (let* ((summary (car s))
;;                                      (hint " => clipboard" )
;;                                      (msg (if (string-match-p "\.\.\.$" summary)
;;                                               (substring summary 0 (- (length summary) (length hint)))
;;                                             msg)))
;;                                 ;; cc actual string
;;                                 (my-pclip (cdr s))
;;                                 ;; echo
;;                                 (message "%s%s" msg hint)))))

(defun my-gclip ()
  "Get clipboard content."
  (let* ((powershell-program (executable-find "powershell.exe")))
    (cond
     ((and (memq system-type '(gnu gnu/linux gnu/kfreebsd))
           powershell-program)
      (string-trim-right
       (with-output-to-string
         (with-current-buffer standard-output
           (call-process powershell-program nil t nil "-command" "Get-Clipboard")))))
     (t
      (xclip-get-selection 'clipboard)))))

(defun my-pclip (str-val)
  "Set clipboard content."
  (let* ((win64-clip-program (executable-find "clip.exe")))
    (cond
     ((and win64-clip-program (memq system-type '(gnu gnu/linux gnu/kfreebsd)))
      (with-temp-buffer
        (insert str-val)
        (call-process-region (point-min) (point-max) win64-clip-program)))
     (t
      (xclip-set-selection 'clipboard str-val)))))


(defun copy-to-x-clipboard ()
  (interactive)
  (let* ((thing (my-use-selected-string-or-ask "")))
    (if (region-active-p) (deactivate-mark))
    (my-pclip thing)))

(defun paste-from-x-clipboard()
  (interactive)
  (let* ((str (my-gclip))
         (fn 'insert))
    (my-delete-selected-region)
    (funcall fn str)))

(defun cut-to-x-clipboard ()
  (interactive)
  (let* ((thing (my-use-selected-string-or-ask "")))
    (my-pclip thing))
  (my-delete-selected-region))


(provide 'init-clipboard)
