(use-package doom-themes
  :init
  ;; (require 'f)
  ;; (load-file "~/.spacemacs.d/modeline.el")
  ;; (add-hook 'find-file-hook 'doom-buffer-mode)
  ;; (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  (load-theme 'doom-one)
  (require 'solaire-mode)
  ;; brighten buffers (that represent real files)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; ...if you use auto-revert-mode:
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  ;; You can do similar with the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  ;; To enable solaire-mode unconditionally for certain modes:
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
  ;; When persp-mode loads a perspective from file, it doesn't restore solaire-mode:
  (advice-add #'persp-load-state-from-file :after #'solaire-mode-restore-persp-mode-buffers)
  ;; (require 'doom-neotree)
  ;; (setq doom-neotree-enable-folder-icons t)
  ;; (setq doom-neotree-enable-file-icons t)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (set-face-attribute 'mode-line nil :box nil))

(provide 'package-doom-theme)
