(use-package solaire-mode
  :after doom-themes
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (advice-add #'persp-load-state-from-file :after #'solaire-mode-restore-persp-mode-buffers))

(provide 'package-solaire-mode)
