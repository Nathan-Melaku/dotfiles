;;

(use-package eshell-up
  :config
  (setq eshell-up-print-parent-dir t))
(use-package eshell-z
  :after eshell)
(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode)
  :init
  (add-hook 'eshell-syntax-highlighting-elisp-buffer-setup-hook #'highlight-quoted-mode))
(use-package shrink-path)
(use-package esh-help)

(provide 'nate-eshell)
;; 
