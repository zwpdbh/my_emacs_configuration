(use-package dap-mode
  :ensure t
  :config
  (progn
    (dap-mode 1)
    (dap-ui-mode 1)
    ;; (dap-tooltip-mode 1)
    ;; (setq tooltip-mode t)

    ;; dap-mode also provides a hydra with dap-hydra
    (add-hook 'dap-stopped-hook
              (lambda (arg) (call-interactively #'dap-hydra)))

    ;; for javascript node debug 
    (require 'dap-node)
    (dap-register-debug-template
     "Node::zwpdbh-debug"
     (list :type "node"
           :cwd nil
           :request "launch"
           :program nil
           :name "Node::zwpdbh-debug"))))

;; Need to call ~dap-node-setup~ for setting up vscode extension.
;; Make sure the ~dap-node-debug-program~ is pointing to the proper file.

(provide 'init-dap)
