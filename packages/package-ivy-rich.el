(use-package ivy-rich
  :after swiper
  :commands ivy-switch-buffer
  :config (progn
            (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
            (setq
             ivy-rich-switch-buffer-align-virtual-buffer nil
             ivy-virtual-abbreviate 'full
             ivy-rich-switch-buffer-align-virtual-buffer t
             ivy-rich-abbreviate-paths t)))

(provide 'package-ivy-rich)
