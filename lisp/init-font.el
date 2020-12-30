(setq zw/favorite-en-fonts '(
                             "Anonymous Pro"
                             "Iosevka Slab"
                             "Consolas"
                             "Terminus (TTF)"
                             "Source Code Pro"
                             "Monaco Nerd Font Mono"
                             "Monaco"
                             "Terminus (TTF) for Windows"
                             "JetBrains Mono"
                             "DejaVu Sans Mono"
                             "Monospace"
                             "Courier New"
                             ))
(setq zw/favorite-cn-fonts '(
                             "Microsoft Yahei"
                             "Microsoft_Yahei"
                             "微软雅黑"
                             "文泉驿等宽微米黑"
                             "黑体"
                             "新宋体"
                             "宋体"
                             ))

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
(zw/set-font 10)

;; set emoji
(when (and (boundp 'global-emojify-mode)
           global-emojify-mode)
  (global-emojify-mode 1))

(provide 'init-font)