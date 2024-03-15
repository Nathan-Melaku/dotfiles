;; Basic configuration
(setq user-full-name "Nathan Melaku"
      user-mail-address "nathanmelaku@protonmail.com")

;; workaround for emacs crashing when started in daemon mode
(setq initial-scratch-message nil)

;; UI related configuration
(setq doom-font (font-spec :family "FiraCode Nerd Font Propo" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 18))
(setq doom-theme 'doom-solarized-dark-high-contrast)
(setq display-line-numbers-type 'relative)
(set-face-attribute 'region nil :background "#001e26")

;; Org related configuration
(setq org-directory "~/Documents/Org/")

;; Lisp goodies
(setq paredit-list '(clojure-mode-hook
                     clojurescript-mode-hook
                     lisp-interaction-mode-hook
                     lisp-mode-hook
                     emacs-lisp-mode-hook))

(defun add-to-multiple-hooks (function hooks)
  "Add a function to multiple hooks"
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(add-to-multiple-hooks 'enable-paredit-mode paredit-list)

;; copilot configuration
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
(setq copilot-indent-offset-warning-disable t)

;; origami configuration
(use-package! origami
  :hook (prog-mode . origami-mode))

;; custom leader key bindings
(map! :leader
      :desc "Toggle code folding" "v f" #'origami-toggle-node
      :desc "Toggle all code folding" "v a" #'origami-toggle-all-nodes)
