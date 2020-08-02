;;;; Automatic fitting minor mode
(defcustom doc-view-autofit-timer-start 1.0
  "Initial value (seconds) for the timer that delays the fitting when
`doc-view-autofit-fit' is called (Which is when a window
configuration change occurs and a document needs to be fitted)."
  :type 'number
  :group 'doc-view)

(defcustom doc-view-autofit-timer-inc 0.02
  "Value to increase (seconds) the timer (see `doc-view-autofit-timer-start')
by, if there is another window configuration change occuring, before
it runs out."
  :type 'number
  :group 'doc-view)

(defcustom doc-view-autofit-default-fit 'width
  "The fitting type initially used when mode is enabled.
Valid values are: width, height, page."
  :type 'symbol
  :group 'doc-view)

(defvar doc-view-autofit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c W") 'doc-view-autofit-width)
    (define-key map (kbd "C-c H") 'doc-view-autofit-height)
    (define-key map (kbd "C-c P") 'doc-view-autofit-page)
    map)
  "Keymap used by `doc-view-autofit-mode'.")

(defun doc-view-autofit-set (type)
  "Set autofitting to TYPE for current buffer."
  (when doc-view-autofit-mode
    (setq doc-view-autofit-type type)
    (doc-view-autofit-fit)))

(defun doc-view-autofit-width ()
  "Set autofitting to width for current buffer."
  (interactive) (doc-view-autofit-set 'width))

(defun doc-view-autofit-height ()
  "Set autofitting to height for current buffer."
  (interactive) (doc-view-autofit-set 'height))

(defun doc-view-autofit-page ()
  "Set autofitting to page for current buffer."
  (interactive) (doc-view-autofit-set 'page))

(defun doc-view-autofit-fit ()
  "Fits the document in the selected window's buffer
delayed with a timer, so multiple calls in succession
don't cause as much overhead."
  (lexical-let
      ((window (selected-window)))
    (if (equal doc-view-autofit-timer nil)
        (setq doc-view-autofit-timer
              (run-with-timer
               doc-view-autofit-timer-start nil
               (lambda ()
                 (if (window-live-p window)
                     (save-selected-window
                       (select-window window)
                       (cancel-timer doc-view-autofit-timer)
                       (setq doc-view-autofit-timer nil)
                       (cond
                        ((equal 'width doc-view-autofit-type)
                         (doc-view-fit-width-to-window))
                        ((equal 'height doc-view-autofit-type)
                         (doc-view-fit-height-to-window))
                        ((equal 'page doc-view-autofit-type)
                         (doc-view-fit-page-to-window))))))))
      (timer-inc-time doc-view-autofit-timer doc-view-autofit-timer-inc))))

(define-minor-mode doc-view-autofit-mode
  "Minor mode for automatic (timer based) fitting in DocView."
  :lighter " AFit" :keymap doc-view-autofit-mode-map :group 'doc-view
  (when doc-view-autofit-mode
    (set (make-local-variable 'doc-view-autofit-type)
         doc-view-autofit-default-fit)
    (set (make-local-variable 'doc-view-autofit-timer) nil)
    (add-hook 'window-configuration-change-hook
              'doc-view-autofit-fit nil t)
    (doc-view-autofit-fit))
  (when (not doc-view-autofit-mode)
    (remove-hook 'window-configuration-change-hook
                 'doc-view-autofit-fit t)
    (when doc-view-autofit-timer
      (cancel-timer doc-view-autofit-timer)
      (setq doc-view-autofit-timer nil))
    (setq doc-view-autofit-type nil)))

(add-hook 'doc-view-mode-hook #'(lambda ()
                                  (doc-view-autofit-mode t)))


(provide 'init-doc-view-auto-fit)