;; indium: javascript awesome development environment
;; https://github.com/NicolasPetton/indium
(use-package indium
  :after js2-mode
  :ensure-system-package (indium . "npm i -g indium")
  ;; :bind (:map js2-mode-map
  ;;         ("C-c C-l" . indium-eval-buffer))
  :hook (((js2-mode typescript-mode) . indium-interaction-mode)))

(provide 'package-indium)
