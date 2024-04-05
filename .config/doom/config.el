;; -*- lexical-binding: t; -*-

;; Basic configuration
(setq user-full-name "Nathan Melaku"
      user-mail-address "nathanmelaku@protonmail.com"
      initial-major-mode 'lisp-interaction-mode
      initial-scratch-message nil
      doom-font (font-spec :family "FiraCode Nerd Font" :size 18 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 20)
      doom-theme 'modus-vivendi
      display-line-numbers-type 'relative
      pixel-scroll-precision-mode t
      org-directory "~/Documents/Org/"
      doom-modeline-position-line-format " "
      doom-modeline-position-column-line-format " "
      doom-modeline-major-mode-icon t)

;; Lisp goodies
(setq paredit-list '(clojure-mode-hook
                     clojurescript-mode-hook
                     lisp-interaction-mode-hook
                     lisp-mode-hook
                     emacs-lisp-mode-hook))

(defun add-to-multiple-hooks (function hooks)
  "Add a function to multiple hooks"
  (mapc (lambda (hook) (add-hook hook function))
        hooks))

(add-to-multiple-hooks 'enable-paredit-mode paredit-list)

;; origami configuration
(use-package! origami
  :hook (prog-mode . origami-mode))

(use-package! god-mode
  :custom
  (god-mode-enable-function-key-translation nil)
  (god-mode-alist '((nil . "C-")
                    ("g" . "M-")
                    ("m" . "C-M-")))
  (god-exempt-major-modes nil)
  (god-exempt-predicates nil)
  (god-exempt-major-modes '(dired-mode magit-status-mode))
  :init
  (god-mode-all))

(defun cursor-change-on-god-mode ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'hbar)))
(add-hook 'post-command-hook #'cursor-change-on-god-mode)

(use-package! spacious-padding
  :config
  (spacious-padding-mode 1)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 8
      :header-line-width 4
      :mode-line-width 6
      :tab-width 4
      :right-divider-width 1
      :scroll-bar-width 8
      :fringe-width 8)))

(after! which-key
  (which-key-enable-god-mode-support))

(after! clojure-mode
  (map! :map clojure-mode-map "C-M-q" #'sp-delete-sexp))

(define-key emacs-lisp-mode-map (kbd "C-M-q") #'sp-delete-sexp)
(define-key lisp-interaction-mode-map (kbd "C-M-q") #'sp-delete-sexp)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; custom leader key bindings
(map!
 :leader
 (:prefix ("t" . "toggle")
  :desc "Toggle code folding" "o" #'origami-toggle-node
  :desc "Toggle all code folding" "O" #'origami-toggle-all-nodes))

(defun god-mode-escape()
  "escape god mode only if it is set"
  (interactive)
  (if god-local-mode
      (doom/escape)
    (god-local-mode)))

;; custom key bindings
(map!
 "<escape>" #'god-mode-escape
 "M-j" #'join-line
 :prefix "C-x"
 "C-1" #'delete-other-windows
 "C-2" #'split-window-below
 "C-3" #'split-window-right
 "C-0" #'delete-window)

;; god mode key bindings
(map!
 :after god-mode
 :map god-local-mode-map
 "i" #'god-local-mode
 "z" #'repeat
 "[" #'backward-paragraph
 "]" #'forward-paragraph)
