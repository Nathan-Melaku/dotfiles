;; nate-evil.el
(use-package goto-chg)

(use-package evil
  :init
  (setq evil-search-module 'evil-search
		evil-ex-complete-emacs-commands nil
		evil-vsplit-window-right t
		evil-split-window-below t
		evil-shift-round nil
		evil-want-integration t
		evil-want-keybinding nil
		evil-want-C-u-scroll t)
  :config
  (evil-mode)

  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit)
  (define-key evil-normal-state-map (kbd "g d") 'xref-find-definitions))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package vimish-fold)

(provide 'nate-evil)
