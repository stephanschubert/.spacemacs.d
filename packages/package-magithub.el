(use-package magithub
  :ensure t
  :after magit
  :init
  ;; FIXME this is a temporary hack; see https://github.com/vermiculus/magithub/issues/299
  (define-error 'ghub-404 "Not Found" 'ghub-http-error)
  :config
  ;; (setq
  ;;  ghub-token (gh-auth-get-oauth-token)
  ;;  ghub-username "jazen")
  (magithub-feature-autoinject t))

(provide 'package-magithub)
