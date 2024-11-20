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

(use-package eshell-prompt-extras)
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))
(defalias 'clear 'eshell/clear 1)

(use-package aweshell
  :straight `(aweshell :type git :host github :repo "manateelazycat/aweshell")
  :config
  (setq eshell-prompt-function #'epe-theme-lambda))

(provide 'nate-eshell)
