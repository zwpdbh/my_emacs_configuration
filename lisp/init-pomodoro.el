(when (maybe-require-package 'pomidor)
  (after-load 'pomidor
    (setq pomidor-sound-tick nil
          pomidor-sound-tack nil)
    (display-line-numbers-mode -1) ; Emacs 26.1+
    (setq left-fringe-width 0 right-fringe-width 0)
    (setq left-margin-width 2 right-margin-width 0)
    ;; force fringe update
    (set-window-buffer nil (current-buffer))))

(provide 'init-pomodoro)