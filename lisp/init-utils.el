;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'cl-lib) ;; (require 'cl) since cl is deprecated

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))



;; From chenbin's code 
(defun require-init (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (or (not maybe-disabled) (not (boundp 'startup-now)))
    (load (file-truename (format "~/.emacs.d/lisp/%s" pkg)) t t)))

(defun local-require (pkg)
  (unless (featurep pkg)
    (load (expand-file-name
           (cond
            ((eq pkg 'go-mode-load)
             (format "~/.emacs.d/site-lisp/go-mode/%s" pkg))
            (t
             (format "~/.emacs.d/site-lisp/%s/%s" pkg pkg))))
          t t)))


(defmacro my-ensure (feature)
  "Make sure FEATURE is required."
  `(unless (featurep ,feature)
     (condition-case nil
         (require ,feature)
       (error nil))))

(defun run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or whole buffer and replace it with cli output."
  (let* ((orig-point (point))
         (b (if (region-active-p) (region-beginning) (point-min)))
         (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e cmd nil t)
    (goto-char orig-point)))

(defun my-use-tags-as-imenu-function-p ()
  "Can use tags file to build imenu function"
  (my-ensure 'counsel-etags)
  (and (locate-dominating-file default-directory "TAGS")
       ;; ctags needs extra setup to extract typescript tags
       (file-exists-p counsel-etags-ctags-options-file)
       (memq major-mode '(typescript-mode
                          js-mode))))

(defun my-add-subdirs-to-load-path (my-lisp-dir)
  "Add sub-directories under MY-LISP-DIR into `load-path'."
  (let* ((default-directory my-lisp-dir))
    (setq load-path
          (append
           (delq nil
                 (mapcar (lambda (dir)
                           (unless (string-match-p "^\\." dir)
                             (expand-file-name dir)))
                         (directory-files "~/.emacs.d/site-lisp/")))
           load-path))))

;; {{ copied from http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun get-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun read-lines (file)
  "Return a list of lines of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))
;; }}

(defun nonempty-lines (s)
  (split-string s "[\r\n]+" t))

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'interpreter-mode-alist (cons pattern mode))))

(defun font-belongs-to (pos fonts)
  "Current font at POS belongs to FONTS."
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar (lambda (f)
                    (member f fonts))
                  fontfaces))))

;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

;; Find the directory containing a given library
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let* ((pattern (concat "^" (file-name-as-directory directory))))
    (if (string-match-p pattern file) file)))

(defun my-prepare-candidate-fit-into-screen (s)
  (let* ((w (frame-width))
         ;; display kill ring item in one line
         (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
    ;; strip the whitespace
    (setq key (replace-regexp-in-string "^[ \t]+" "" key))
    ;; fit to the minibuffer width
    (if (> (length key) w)
        (setq key (concat (substring key 0 (- w 4)) "...")))
    (cons key s)))

(defun my-select-from-kill-ring (fn)
  "If N > 1, yank the Nth item in `kill-ring'.
If N is nil, use `ivy-mode' to browse `kill-ring'."
  (interactive "P")
  (let* ((candidates (cl-remove-if
                      (lambda (s)
                        (or (< (length s) 5)
                            (string-match-p "\\`[\n[:blank:]]+\\'" s)))
                      (delete-dups kill-ring)))
         (ivy-height (/ (frame-height) 2)))
    (ivy-read "Browse `kill-ring':"
              (mapcar #'my-prepare-candidate-fit-into-screen candidates)
              :action fn)))

(defun my-delete-selected-region ()
  "Delete selected region."
  (when (region-active-p)
    (delete-region (region-beginning) (region-end))))

(defun my-insert-str (str)
  "Insert STR into current buffer."
  ;; ivy8 or ivy9
  (if (consp str) (setq str (cdr str)))
  ;; evil-mode?
  (if (and (functionp 'evil-normal-state-p)
           (boundp 'evil-move-cursor-back)
           (evil-normal-state-p)
           (not (eolp))
           (not (eobp)))
      (forward-char))

  (my-delete-selected-region)

  ;; insert now
  (insert str)
  str)

(defun my-line-str (&optional line-end)
  (buffer-substring-no-properties (line-beginning-position)
                                  (if line-end line-end (line-end-position))))

(defun my-is-in-one-line (b e)
  (save-excursion
    (goto-char b)
    (and (<= (line-beginning-position) b)
         (<= e (line-end-position)))))

(defun my-buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun my-selected-str ()
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun my-use-selected-string-or-ask (&optional hint)
  "Use selected region or ask user input for string."
  (if (region-active-p) (my-selected-str)
    (if (or (not hint) (string= "" hint)) (thing-at-point 'symbol)
      (read-string hint))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defvar load-user-customized-major-mode-hook t)
(defvar cached-normal-file-full-path nil)

(defun buffer-too-big-p ()
  ;; 5000 lines
  (> (buffer-size) (* 5000 80)))

(defun file-too-big-p (file)
  (> (nth 7 (file-attributes file))
     (* 5000 64)))

(defvar force-buffer-file-temp-p nil)
(defun is-buffer-file-temp ()
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file."
  (interactive)
  (let* ((f (buffer-file-name)) (rlt t))
    (cond
     ((not load-user-customized-major-mode-hook)
      (setq rlt t))
     ((not f)
      ;; file does not exist at all
      ;; org-babel edit inline code block need calling hook
      (setq rlt nil))
     ((string= f cached-normal-file-full-path)
      (setq rlt nil))
     ((string-match (concat "^" temporary-file-directory) f)
      ;; file is create from temp directory
      (setq rlt t))
     ((and (string-match "\.html$" f)
           (file-exists-p (replace-regexp-in-string "\.html$" ".org" f)))
      ;; file is a html file exported from org-mode
      (setq rlt t))
     (force-buffer-file-temp-p
      (setq rlt t))
     (t
      (setq cached-normal-file-full-path f)
      (setq rlt nil)))
    rlt))

(defvar my-mplayer-extra-opts ""
  "Extra options for mplayer (ao or vo setup).  For example,
you can '(setq my-mplayer-extra-opts \"-ao alsa -vo vdpau\")'.")

(defun my-guess-mplayer-path ()
  (let* ((rlt "mplayer"))
    (cond
     (*is-a-mac* (setq rlt "mplayer -quiet"))
     (*linux*
      (setq rlt (format "mplayer -quiet -stop-xscreensaver %s" my-mplayer-extra-opts)))
     (*cygwin*
      (if (file-executable-p "/cygdrive/c/mplayer/mplayer.exe")
          (setq rlt "/cygdrive/c/mplayer/mplayer.exe -quiet")
        (setq rlt "/cygdrive/d/mplayer/mplayer.exe -quiet")))
     (t ; windows
      (if (file-executable-p "c:\\\\mplayer\\\\mplayer.exe")
          (setq rlt "c:\\\\mplayer\\\\mplayer.exe -quiet")
        (setq rlt "d:\\\\mplayer\\\\mplayer.exe -quiet"))))
    rlt))

(defun my-guess-image-viewer-path (file &optional is-stream)
  (let* ((rlt "mplayer"))
    (cond
     (*is-a-mac*
      (setq rlt
            (format "open %s &" file)))
     (*linux*
      (setq rlt
            (if is-stream (format "curl -L %s | feh -F - &" file) (format "feh -F %s &" file))))
     (*cygwin* (setq rlt "feh -F"))
     (t ; windows
      (setq rlt
            (format "rundll32.exe %s\\\\System32\\\\\shimgvw.dll, ImageView_Fullscreen %s &"
                    (getenv "SystemRoot")
                    file))))
    rlt))

(defun should-use-minimum-resource ()
  "Some files should use minimum resource (no syntax highlight, no line number display)."
  (and buffer-file-name
       (string-match-p "\.\\(mock\\|min\\|bundle\\)\.js" buffer-file-name)))

(defun my-async-shell-command (command)
  "Execute string COMMAND asynchronously."
  (let* ((proc (start-process "Shell"
                              nil
                              shell-file-name
                              shell-command-switch command)))
    (set-process-sentinel proc `(lambda (process signal)
                                  (let* ((status (process-status process)))
                                    (when (memq status '(exit signal))
                                      (unless (string= (substring signal 0 -1) "finished")
                                        (message "Failed to run \"%s\"." ,command))))))))

;; set encoding 
(set-language-environment "UTF-8")

;; remove "^M" from end of line in file; see https://www.emacswiki.org/emacs/EndOfLineTips
(defun no-junk-please-were-unixish ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))
(add-hook 'find-file-hooks 'no-junk-please-were-unixish)


;; solve problem assert function definition is void
(defmacro assert (test-form)
  `(when (not ,test-form)
     (error "Assertion failed: %s" (format "%s" ',test-form))))


;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned/52789#52789
;; How to indent keywords aligned in emacs-lisp
(defun void~calculate-lisp-indent (&optional parse-start)
  "Add better indentation for quoted and backquoted lists."
  ;; This line because `calculate-lisp-indent-last-sexp` was defined with `defvar` 
  ;; with it's value ommited, marking it special and only defining it locally. So  
  ;; if you don't have this, you'll get a void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or
                      ;; Containing sexp has nothing before this line
                      ;; except the first element. Indent under that element.
                      (= (point) calculate-lisp-indent-last-sexp)

                      ;; First sexp after `containing-sexp' is a keyword. This
                      ;; condition is more debatable. It's so that I can have
                      ;; unquoted plists in macros. It assumes that you won't
                      ;; make a function whose name is a keyword.
                      ;; (when-let (char-after (char-after (1+ containing-sexp)))
                      ;;   (char-equal char-after ?:))

                      ;; Check for quotes or backquotes around.
                      (let* ((positions (elt state 9))
                             (last (car (last positions)))
                             (rest (reverse (butlast positions)))
                             (any-quoted-p nil)
                             (point nil))
                        (or
                         (when-let (char (char-before last))
                           (or (char-equal char ?')
                               (char-equal char ?`)))
                         (progn
                           (while (and rest (not any-quoted-p))
                             (setq point (pop rest))
                             (setq any-quoted-p
                                   (or
                                    (when-let (char (char-before point))
                                      (or (char-equal char ?')
                                          (char-equal char ?`)))
                                    (save-excursion
                                      (goto-char (1+ point))
                                      (looking-at-p
                                       "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                           any-quoted-p))))
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                     (line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

;;; color hex color string
;;; http://ergoemacs.org/emacs/emacs_CSS_colors.html
(defun zw/syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-03-12"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background
                      (let* (
                             (ms (match-string-no-properties 0))
                             (r (substring ms 1 2))
                             (g (substring ms 2 3))
                             (b (substring ms 3 4)))
                        (concat "#" r r g g b b))))))
     ("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush))

;; show hex color string's corresponding color
(add-hook 'emacs-lisp-mode-hook 'zw/syntax-color-hex)

(provide 'init-utils)
;;; init-utils.el ends here
