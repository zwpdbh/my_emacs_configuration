(setq zw/favorite-en-fonts '(
                             "Monaco"
                             "Monaco Nerd Font Mono"                             
                             "Source Code Pro"
                             "Iosevka Slab"
                             "Iosevka Curly Slab"
                             "Anonymous Pro"
                             "Inconsolata"
                             "Consolas"                             
                             "Ubuntu Mono"                             
                             "Terminus (TTF)"
                             "Terminus (TTF) for Windows"
                             "JetBrains Mono"
                             "DejaVu Sans Mono"
                             "Monospace"
                             "Courier New"))

;; use terminal preferred fonts
(unless (display-graphic-p)
  (setq zw/favorite-en-fonts (delete "Terminus (TTF)" zw/favorite-en-fonts))
  (setq zw/favorite-en-fonts (delete "Terminus (TTF) for Windows" zw/favorite-en-fonts))
  (setq zw/favorite-en-fonts (cons "Terminus (TTF)" zw/favorite-en-fonts))
  (setq zw/favorite-en-fonts (cons "Terminus (TTF) for Windows" zw/favorite-en-fonts)))

;; For cases which Chinese characters could not be displayed properly:
;; 1) Ensure Chinese characters are installed in System.
;; 2) Make sure terminal env from system can display CN-font properly: For example, display file content with CN characters in terminal (powershell in Windows).
;; 3) Make sure Emacs's env inherited from the terminal's env from which Emacs is launched (step 2 is important).
;;    If you changed terminal's env, you need to close and re-open terminal, then start Emacs to test.
;; 4) Make sure Emacs use utf-8 for default encoding.
;; If everyting works well, the following characters should be displayed properly.
(setq zw/favorite-cn-fonts '(
                             "Microsoft Yahei"
                             "Microsoft_Yahei"
                             "微软雅黑"
                             "文泉驿等宽微米黑"
                             "黑体"
                             "新宋体"
                             "宋体"))

;; Given a list of fonts, select the first one available. So, put favorite font at the beginning of the list
(defun zw/select-available-font (my-fonts)
  (let ((result nil))
    (catch 'response
      (dolist (current-font my-fonts)
        (when (member current-font (font-family-list))
          (setq result current-font)
          (throw 'response result))))
    result))

;; make font setting string including its size
(defun zw/make-font-string (font-name font-size)
  ;; TODO scale the font size based on font 
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s-%s" font-name font-size)))

(defun zw/set-font (font-size)
  (let ((en-font (zw/make-font-string (zw/select-available-font zw/favorite-en-fonts) font-size))
        (cn-font (zw/make-font-string (zw/select-available-font zw/favorite-cn-fonts) font-size)))
    
    (set-face-attribute 'default nil :font en-font)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset cn-font))))

;; set font
(setq zw/font-size 10)
(if (display-graphic-p)
    (zw/set-font zw/font-size)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when (display-graphic-p frame)
                (with-selected-frame frame
                  (zw/set-font zw/font-size))))))

;; Adjust font in every buffer instead of having to zoom in each buffer separately.
;; ref: https://www.emacswiki.org/emacs/GlobalTextScaleMode
(when (maybe-require-package 'default-text-scale)
  (global-set-key (kbd "C-x C-0") #'default-text-scale-reset)
  (global-set-key (kbd "C-x C-=") #'default-text-scale-increase)
  (global-set-key (kbd "C-x C--") #'default-text-scale-decrease))


;; set emoji
(when (and (boundp 'global-emojify-mode)
           global-emojify-mode)
  (global-emojify-mode 1))

(provide 'init-font)