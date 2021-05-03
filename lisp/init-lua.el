;; ref: https://github.com/immerrr/lua-mode
;; ref: https://develop.spacemacs.org/layers/+lang/lua/README.html
(when (maybe-require-package 'lua-mode)
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(provide 'init-lua)