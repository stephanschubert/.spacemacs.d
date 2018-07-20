(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init
   '(
     ivy-mode
     magit-todos
     rjsx-mode)))

(provide 'package-evil-collection)
