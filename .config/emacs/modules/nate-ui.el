;; nate-ui.el

;; disable toolbar, scroll bar, and menu bar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default tab-width 4)
;; set font
(set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font Mono" :height 150)
(set-frame-parameter nil 'alpha-background 95)
(blink-cursor-mode -1)

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package vertico-posframe
  :config
  (setq vertico-posframe-parameters
		'((left-fringe . 8)
          (right-fringe . 8)))
  (vertico-posframe-mode 1))

(provide 'nate-ui)
