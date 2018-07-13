;; (require 'f)
;; (load-file "~/.spacemacs.d/modeline.el")
;; (add-hook 'find-file-hook 'doom-buffer-mode)
;; (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
;; (require 'doom-neotree)
;; (setq doom-neotree-enable-folder-icons t)
;; (setq doom-neotree-enable-file-icons t)

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  (progn
    (set-face-attribute 'mode-line-inactive nil :box nil)
    (set-face-attribute 'mode-line nil :box nil)
    (doom-themes-neotree-config)
    (doom-themes-org-config)))

(provide 'package-doom-theme)
