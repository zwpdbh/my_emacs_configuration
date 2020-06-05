;; ===== set font 
;; https://github.com/adobe-fonts/source-code-pro
;; to adjust font dynamically
;; C-xC-+ and C-xC-- to increase or decrease the buffer text size
(set-face-attribute 'default nil :height 110)

(cond
 ((member "Source Code Pro" (font-family-list))
  (set-frame-font "Source Code Pro"))
 ((and *win64* (member "Terminus (TTF) for Windows" (font-family-list)))
  (set-frame-font "Terminus (TTF) for Windows"))
 ((member "Terminus (TTF)" (font-family-list))
  (print "hello")
  (set-frame-font "Terminus (TTF)")))
;; check current font are using: http://ergoemacs.org/emacs/emacs_list_and_set_font.html
;; terminus font: https://files.ax86.net/terminus-ttf/#what


(provide 'init-font)