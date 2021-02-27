;; From https://manateelazycat.github.io/emacs/2020/03/22/emacs-rime.html
;; sudo apt install fcitx-rime -y

;; use config file from ~/.emacs.d/fcitx/rime as directory for holding rime configuration
(when (maybe-require-package 'rime)
  (setq rim-user-data-dir "~/.emacs.d/fcitx/rime")
  
  ;; (setq rime-posframe-properties
  ;;       (list :background-color "#333333"
  ;;             :foreground-color "#dcdccc"
  ;;             :font "WenQuanYi Micro Hei Mono-14"
  ;;             :internal-border-width 10))
  
  (setq default-input-method "rime"
        rime-show-candidate 'posframe))

(provide 'init-pinyin)

