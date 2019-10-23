(use-package selected
  :diminish selected-minor-mode
  :ensure t
  :config
  (setq-default selected-minor-mode-override t)
  (selected-global-mode t)
  :bind (:map selected-keymap
	        ;; ("n" . narrow-to-region)
	        ;; (";" . comment-dwim-2)
          ;; ("$" . flyspell-region)
          ;; ("u" . upcase-region)
          ;; ("d" . downcase-region)
          ("C-c s c" . count-words-region)
          ;; ("\\" . indent-region)
          ;; ("w" . copy-region-as-kill)
          ;; ("W" . copy-as-format)
          ;; ("k" . kill-region)
          ("C-c s m" . apply-macro-to-region-lines)))

(provide 'package-selected)
