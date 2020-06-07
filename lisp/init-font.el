;; ===== set font
;; use (font-family-list) to check all the fonts available for emacs.
;; https://github.com/adobe-fonts/source-code-pro
;; C-xC-+ and C-xC-- to increase or decrease the buffer text size
;; (set-face-attribute 'default nil :height 110)

;; test fonts 
;; (set-face-attribute 'default nil :family "Anonymous Pro" :height 100)
;; (set-face-attribute 'default nil :family "Monospace" :height 100)
;; (set-face-attribute 'default nil :family "Space Mono" :height 100)
;; (set-face-attribute 'default nil :family "Monaco" :height 100)
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 100)
;; (set-face-attribute 'default nil :family "Monaco Nerd Font Mono" :height 110)


;; (cond
;;  ((member "Anonymous Pro" (font-family-list))
;;   (set-frame-font "Anonymous Pro"))
;;  ((member "Source Code Pro" (font-family-list))
;;   (set-frame-font "Source Code Pro"))
;;  ((and *win64* (member "Terminus (TTF) for Windows" (font-family-list)))
;;   (set-frame-font "Terminus (TTF) for Windows"))
;;  ((member "Terminus (TTF)" (font-family-list))
;;   (print "hello")
;;   (set-frame-font "Terminus (TTF)")))
;; check current font are using: http://ergoemacs.org/emacs/emacs_list_and_set_font.html
;; terminus font: https://files.ax86.net/terminus-ttf/#what

;; The below font is set from
;; https://raw.githubusercontent.com/baohaojun/system-config/master/.emacs_d/lisp/bhj-fonts.el

(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))

(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s-%s" font-name font-size)))

(defvar bhj-english-font-size nil)
(defvar chinese-font-size-scale-alist nil)


(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-fonts-scale)
  (setq chinese-fonts-scale (or chinese-fonts-scale 1.2))
  (setq face-font-rescale-alist `(("Microsoft Yahei" . ,chinese-fonts-scale)
                                  ("Microsoft_Yahei" . ,chinese-fonts-scale)
                                  ("微软雅黑" . ,chinese-fonts-scale)
                                  ("WenQuanYi Zen Hei" . ,chinese-fonts-scale)))
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl)                         ; for find if
  (setq bhj-english-font-size english-font-size)
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts))))

    ;; Set the default English font
    ;;
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (set-face-attribute
     'default nil :font en-font)
    (condition-case font-error
        (progn
          (set-face-font 'italic (font-spec :family "JetBrains Mono" :slant 'italic :weight 'normal :size (+ 0.0 english-font-size)))
          (set-face-font 'bold-italic (font-spec :family "JetBrains Mono" :slant 'italic :weight 'bold :size (+ 0.0 english-font-size)))

          (set-fontset-font t 'symbol (font-spec :family "JetBrains Mono")))
      (error nil))
    (set-fontset-font t 'symbol (font-spec :family "Unifont") nil 'append)
    (set-fontset-font t nil (font-spec :family "DejaVu Sans"))

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset zh-font)))
  (when (and (boundp 'global-emojify-mode)
             global-emojify-mode)
    (global-emojify-mode 1)))


(defvar bhj-english-fonts '("Monaco Nerd Font Mono"
                            "Monaco"
                            "Inconsolata"
                            "Anonymous Pro"
                            "Source Code Pro"
                            "Terminus (TTF) for Windows"
                            "Terminus (TTF)"
                            "Consolas"
                            "JetBrains Mono"
                            "DejaVu Sans Mono"
                            "Monospace"
                            "Courier New"))
(defvar bhj-chinese-fonts '("Microsoft Yahei"
                            "Microsoft_Yahei"
                            "微软雅黑"
                            "文泉驿等宽微米黑"
                            "黑体"
                            "新宋体"
                            "宋体"))

(qiang-set-font bhj-english-fonts
                10
                bhj-chinese-fonts
                0.9)


;; On different platforms, I need to set different scaling rate for
;; differnt font size.
(cond
 ((and (boundp '*is-a-mac*) *is-a-mac*)
  (setq chinese-font-size-scale-alist '((10.5 . 1.3) (11.5 . 1.3) (16 . 1.3) (18 . 1.25))))
 ((and (boundp '*is-a-win*) *is-a-win*)
  (setq chinese-font-size-scale-alist '((11.5 . 1.25) (16 . 1.25))))
 (t ;; is a linux:-)
  (setq chinese-font-size-scale-alist '((12 . 1.25) (12.5 . 1.25) (14 . 1.20) (16 . 1.25) (20 . 1.20)))))

(defvar bhj-english-font-size-steps '(9 10.5 11.5 12 12.5 13 14 16 18 20 22 40))
(defun bhj-step-frame-font-size (step)
  (let ((steps bhj-english-font-size-steps)
        next-size)
    (when (< step 0)
      (setq steps (reverse bhj-english-font-size-steps)))
    (setq next-size
          (cadr (member bhj-english-font-size steps)))
    (when next-size
      (qiang-set-font bhj-english-fonts next-size bhj-chinese-fonts (cdr (assoc next-size chinese-font-size-scale-alist)))
      (message "Your font size is set to %.1f" next-size))))

(global-set-key [(control x) (meta -)] (lambda () (interactive) (bhj-step-frame-font-size -1)))
(global-set-key [(control x) (meta +)] (lambda () (interactive) (bhj-step-frame-font-size 1)))

(set-face-attribute 'default nil :font (font-spec))


;;; Refs
;; http://baohaojun.github.io/perfect-emacs-chinese-font.html (狠狠地折腾了一把Emacs中文字体)
;; http://blog.chinaunix.net/uid-11187-id-3040030.html 关于字符集和乱码的思考

(provide 'init-font)