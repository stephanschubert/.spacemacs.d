(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init
   '(
     magit-todos
     rjsx-mode)))

(provide 'package-evil-collection)
