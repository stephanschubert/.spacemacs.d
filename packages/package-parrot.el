(use-package parrot
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-r") 'parrot-rotate-next-word-at-point)
  (define-key evil-normal-state-map (kbd "C-S-r") 'parrot-rotate-prev-word-at-point)

  (setq-default parrot-rotate-dict
    '(
       (:rot ("it" "it.skip" "it.only"))
       (:rot ("test" "test.skip" "test.only"))
       (:rot ("describe" "describe.skip" "describe.only"))
       (:rot ("&&" "||"))
       (:rot ("===" "!=="))))

  (parrot-mode))

(provide 'package-parrot)
