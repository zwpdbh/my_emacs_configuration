;; Need to call ~dap-node-setup~ for setting up vscode extension.
;; Make sure the ~dap-node-debug-program~ is pointing to the proper file.

(when (symbol-function 'dap-node)
  (when (maybe-require-package 'dap-mode)
    ;; for javascript node debug 
    (require 'dap-node)
    (dap-register-debug-template
     "Node::zwpdbh-debug"
     (list :type "node"
           :cwd nil
           :request "launch"
           :program nil
           :name "Node::zwpdbh-debug")))

  ;; dap-mode also provides a hydra with dap-hydra
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))

  (add-hook 'dap-mode-hook '(lambda ()
                              (dap-ui-mode 1))))


(provide 'init-dap)
