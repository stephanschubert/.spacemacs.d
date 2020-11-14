(use-package evil
  :ensure t
  :init
  ;; This is optional since it's already set to t by default.
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init
   '(
     ivy-mode
     magit-todos
     rjsx-mode)))

(provide 'package-evil-collection)
