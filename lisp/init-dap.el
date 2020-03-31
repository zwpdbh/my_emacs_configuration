(when (maybe-require-package 'dap-mode)
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
         :name "Node::zwpdbh-debug"))
  (add-hook 'dap-mode-hook '(lambda ()
                              (dap-mode 1)
                              (dap-ui-mode 1))))

;; Need to call ~dap-node-setup~ for setting up vscode extension.
;; Make sure the ~dap-node-debug-program~ is pointing to the proper file.

(provide 'init-dap)
