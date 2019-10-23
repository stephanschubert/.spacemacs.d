(use-package historian
  :ensure t)

(use-package ivy-historian
  :ensure t
  :after ivy
  :init
  (historian-mode +1)
  (setq ivy-historian-recent-boost most-positive-fixnum)
  :config
  (ivy-historian-mode +1))

(provide 'package-ivy-historian)
