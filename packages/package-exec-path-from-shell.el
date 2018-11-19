(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

(provide 'package-exec-path-from-shell)
