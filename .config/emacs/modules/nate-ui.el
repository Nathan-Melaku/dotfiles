;; nate-ui.el

;; disable toolbar, scroll bar, and menu bar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default tab-width 4)
;; ;; set font
(add-to-list 'default-frame-alist '(font . "JetBrainsMonoNL Nerd Font Mono-18" ))
(add-to-list 'default-frame-alist '(alpha-background . 98))
(blink-cursor-mode -1)

(defun nate/fill-column ()
  (setq fill-column 90)
  (display-fill-column-indicator-mode ))
(add-hook 'prog-mode-hook #'nate/fill-column)

(defun nate/transparent ()
  (interactive)
(set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font Mono" :height 150)
(set-frame-parameter nil 'alpha-background 95))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-palenight t))

(use-package minions
  :config (minions-mode 1))

(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq x-underline-at-descent-line t))

;; (use-package mood-line
;;   :config
;;   (mood-line-mode)
;;   :custom
;;   (mood-line-glyph-alist mood-line-glyphs-unicode))

(use-package vertico-posframe
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (vertico-posframe-mode 1))

(provide 'nate-ui)
