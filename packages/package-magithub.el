(use-package magithub
  :after magit
  :config
  (setq
   ghub-token (gh-auth-get-oauth-token)
   ghub-username "jazen")
  (magithub-feature-autoinject t))


(provide 'package-magithub)
