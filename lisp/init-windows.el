;; -*- coding: utf-8; lexical-binding: t; -*-

;; change the default split-screen direction
(defun zw/split-window-vertically-by-default ()
  (interactive)
  (setq split-width-threshold nil))

(defun zw/split-window-horizontally-by-default ()
  (interactive)
  (setq split-width-threshold 1))

(zw/split-window-vertically-by-default)

(defvar my-ratio-dict
  '((1 . 1.61803398875)
    (2 . 2)
    (3 . 3)
    (4 . 4)
    (5 . 0.61803398875))
  "The ratio dictionary.")

(defun my-split-window-horizontally (&optional ratio)
  "Split window horizontally and resize the new window.
'C-u number M-x my-split-window-horizontally' uses pre-defined
ratio from `my-ratio-dict'.
Always focus on bigger window."
  (interactive "P")
  (let* (ratio-val)
    (cond
     (ratio
      (setq ratio-val (cdr (assoc ratio my-ratio-dict)))
      (split-window-horizontally (floor (/ (window-body-width)
                                           (1+ ratio-val)))))
     (t
      (split-window-horizontally)))
    (set-window-buffer (next-window) (current-buffer))
    (if (or (not ratio-val)
            (>= ratio-val 1))
        (windmove-right))))

(defun my-split-window-vertically (&optional ratio)
  "Split window vertically and resize the new window.
'C-u number M-x my-split-window-vertically' uses pre-defined
ratio from `my-ratio-dict'.
Always focus on bigger window."
  (interactive "P")
  (let* (ratio-val)
    (cond
     (ratio
      (setq ratio-val (cdr (assoc ratio my-ratio-dict)))
      (split-window-vertically (floor (/ (window-body-height)
                                         (1+ ratio-val)))))
     (t
      (split-window-vertically)))
    ;; open another window with current-buffer
    (set-window-buffer (next-window) (current-buffer))
    ;; move focus if new window bigger than current one
    (if (or (not ratio-val)
            (>= ratio-val 1))
        (windmove-down))))

(global-set-key (kbd "C-x 2") 'my-split-window-vertically)
(global-set-key (kbd "C-x 3") 'my-split-window-horizontally)

;; ========================
;; configuration related with window layout
;; ========================
;; https://emacs.stackexchange.com/questions/2710/switching-between-window-layouts
;; Use C-x r w a to save the current window configuration into register a.
;; Use C-x r j a to pop back to the saved window configuration in a.
(winner-mode 1)
(defun toggle-full-window()
  "Toggle full view of selected window."
  (interactive)
  ;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))


;; {{ move focus between sub-windows
(setq winum-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
        (define-key map (kbd "M-1") 'winum-select-window-1)
        (define-key map (kbd "M-2") 'winum-select-window-2)
        (define-key map (kbd "M-3") 'winum-select-window-3)
        (define-key map (kbd "M-4") 'winum-select-window-4)
        (define-key map (kbd "M-5") 'winum-select-window-5)
        (define-key map (kbd "M-6") 'winum-select-window-6)
        (define-key map (kbd "M-7") 'winum-select-window-7)
        (define-key map (kbd "M-8") 'winum-select-window-8)
        map))

(use-package winum
  :ensure t)

(eval-after-load 'winum
  '(progn
     (setq winum-format "%s")
     (setq winum-mode-line-position 0)
     (set-face-attribute 'winum-face nil :foreground "DeepPink" :underline "DeepPink" :weight 'bold)
     (winum-mode 1)))
;; }}


;; https://github.com/abo-abo/ace-window
;; `M-x ace-window ENTER m` to swap window
;; (global-set-key (kbd "C-x o") 'ace-window)
(use-package ace-window
  :ensure t
  :init
  :config
  (progn
    (setq aw-scope 'frame)
    (global-set-key (kbd "C-x O") 'other-frame)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 1.5)))))))

;; Use to quickly transpose A|B(horizontally) to A/B(vertically) for window split
(when (maybe-require-package 'transpose-frame)
  (defun zw/toggle-window-split ()
    "Compile Erlang module in current buffer."
    (interactive)
    (transpose-frame)))


(provide 'init-windows)
