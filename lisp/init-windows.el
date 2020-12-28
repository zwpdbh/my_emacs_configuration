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


(when (maybe-require-package 'workgroups2)
  ;; (setq wg-use-default-session-file nil)
  ;; don't open last workgroup automatically in `wg-open-session',
  ;; I only want to check available workgroups! Nothing more.
  (setq wg-load-last-workgroup nil)
  (setq wg-open-this-wg nil)

  ;;(workgroups-mode 1) ; put this one at the bottom of .emacs
  ;; by default, the sessions are saved in "~/.emacs_workgroups"
  (autoload 'wg-create-workgroup "workgroups2" nil t)

  (defun my-wg-switch-workgroup ()
    (interactive)
    (let (group-names selected-group)
      (unless (featurep 'workgroups2)
        (require 'workgroups2))
      (setq group-names
            (mapcar (lambda (group)
                      ;; re-shape list for the ivy-read
                      (cons (wg-workgroup-name group) group))
                    (wg-session-workgroup-list (read (f-read-text (file-truename wg-session-file))))))
      (ivy-read "work groups" group-names
                :action (lambda (group)
                          (wg-find-session-file wg-default-session-file)
                          (wg-switch-to-workgroup group)))))


  (eval-after-load 'workgroups2
    '(progn
       ;; make sure wg-create-workgroup always success
       (defadvice wg-create-workgroup (around wg-create-workgroup-hack activate)
         (unless (file-exists-p (wg-get-session-file))
           (wg-reset t)
           (wg-save-session t))

         (unless wg-current-session
           ;; code extracted from `wg-open-session'.
           ;; open session but do NOT load any workgroup.
           (let ((session (read (f-read-text (file-truename wg-session-file)))))
             (setf (wg-session-file-name session) wg-session-file)
             (wg-reset-internal (wg-unpickel-session-parameters session))))
         ad-do-it
         ;; save the session file in real time
         (wg-save-session t))

       (defadvice wg-reset (after wg-reset-hack activate)
         (wg-save-session t))

       ;; I'm fine to to override the original workgroup
       (defadvice wg-unique-workgroup-name-p (around wg-unique-workgroup-name-p-hack activate)
         (setq ad-return-value t)))))


;; (defun rotate-windows ()
;;   "Rotate windows in clock-wise direction."
;;   (interactive)
;;   (cond ((not (> (count-windows)1))
;;          (message "You can't rotate a single window!"))
;;         (t
;;          (setq i 1)
;;          (setq numWindows (count-windows))
;;          (while (< i numWindows)
;;            (let* (
;;                   (w1 (elt (window-list) i))
;;                   (w2 (elt (window-list) (+ (% i numWindows) 1)))

;;                   (b1 (window-buffer w1))
;;                   (b2 (window-buffer w2))

;;                   (s1 (window-start w1))
;;                   (s2 (window-start w2))
;;                   )
;;              (set-window-buffer w1 b2)
;;              (set-window-buffer w2 b1)
;;              (set-window-start w1 s2)
;;              (set-window-start w2 s1)
;;              (setq i (1+ i)))))))

;; resize bindings
;; (global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "C-s-<down>") 'shrink-window)
;; (global-set-key (kbd "C-s-<up>") 'enlarge-window)




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

(provide 'init-windows)
