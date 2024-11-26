;; nate-ui.el

;; disable toolbar, scroll bar, and menu bar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default tab-width 4)
;; ;; set font
(add-to-list 'default-frame-alist '(font . "JetBrainsMonoNL Nerd Font Mono-18" ))
(add-to-list 'default-frame-alist '(alpha-background . 99))
(blink-cursor-mode -1)

(defun nate/fill-column ()
  (setq fill-column 120)
  (display-fill-column-indicator-mode ))
(add-hook 'prog-mode-hook #'nate/fill-column)

(defun nate/transparent ()
  (interactive)
(set-face-attribute 'default nil :font "JetBrainsMonoNL Nerd Font Mono" :height 150)
(set-frame-parameter nil 'alpha-background 95))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-snazzy t))

(use-package minions
  :config (minions-mode 1))

(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq x-underline-at-descent-line t))

(use-package vertico-posframe
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (vertico-posframe-mode 1))

(use-package pulsar
  :bind (("M-SPC t p" . pulsar-pulse-line)
         ("M-SPC t P" . pulsar-highlight-line))
  :config
  (setq pulsar-pulse t
        pulsar-dely 0.08
        pulsar-iterations 10
        pulsar-face 'pulsar-cyan
        pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

(provide 'nate-ui)
