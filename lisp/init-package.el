(use-package package
  :bind (:map package-menu-mode-map
              ("t" . #'package-menu-upgrade-package))
  :config
  (defun package-menu-upgrade-package ()
    "Mark current package for upgrading (i.e. also mark obsolete version for deletion.)"
    (interactive)
    (when-let ((upgrades (package-menu--find-upgrades))
               (description (tabulated-list-get-id))
               (name (package-desc-name description))
               (upgradable (cdr (assq name upgrades))))
      ;; Package is upgradable
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((current-description (tabulated-list-get-id))
                 (current-name (package-desc-name current-description)))
            (when (equal current-name name)
              (cond ((equal description current-description)
                     (package-menu-mark-install)
                     (forward-line -1))
                    (t (package-menu-mark-delete)))))
          (forward-line 1))))))

(provide 'init-package)