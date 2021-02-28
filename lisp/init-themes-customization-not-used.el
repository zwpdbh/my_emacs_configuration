;; make weyland theme use normal font size for header-line
(set-face-attribute 'header-line nil
                    :height 1.0
                    :underline nil)

;; set highlight face
(set-face-attribute 'highlight nil
                    :weight 'bold
                    :foreground "#86dc2f"
                    :background (face-background 'default t t))

;; disable fringe 
(set-fringe-mode 0)