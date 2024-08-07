;;; Package --- Summery
;;; Commentary:
;; -*- lexical-binding: t; -*-

;;; Code:
;; Basic configuration
(setq user-full-name "Nathan Melaku"
      user-mail-address "nathanmelaku@protonmail.com"
      initial-major-mode 'lisp-interaction-mode
      initial-scratch-message nil
      doom-font (font-spec :family "FiraCode Nerd Font Propo" :size 24 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 26)
      doom-theme 'modus-vivendi-tinted
      display-line-numbers-type 'relative
      pixel-scroll-precision-mode t
      org-directory "~/Documents/Org/"
      doom-modeline-major-mode-icon t
      modus-themes-bold-constructs t)

(add-to-list 'default-frame-alist '(alpha-background . 85))

;; Lisp goodies
(setq paredit-list '(clojure-mode-hook
                     clojurescript-mode-hook
                     lisp-interaction-mode-hook
                     lisp-mode-hook
                     emacs-lisp-mode-hook))

(defun add-to-multiple-hooks (function hooks)
  "Add a FUNCTION to multiple HOOKS."
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
  :config
  (mapc (lambda (mode) (add-to-list 'god-exempt-major-modes mode))
        '(vterm-mode eshell-mode))
  :init
  (god-mode-all))

(defvar cursor-bar-list '(vterm-mode eshell-mode))
(defun cursor-change-on-god-mode ()
  (setq cursor-type
        (cond
         ((member major-mode cursor-bar-list) 'bar)
         ((or god-local-mode buffer-read-only) 'box)
         (t 'hbar))))
(add-hook 'post-command-hook #'cursor-change-on-god-mode)

(use-package! spacious-padding
  :config
  (spacious-padding-mode 1)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 2
      :header-line-width 4
      :mode-line-width 2
      :tab-width 4
      :right-divider-width 2
      :scroll-bar-width 0
      :fringe-width 4)))

(use-package! projectile
  :config
  (setq projectile-project-search-path
        '("~/Projects")))

;; After blocks
(after! which-key
  (which-key-enable-god-mode-support))

(after! clojure-mode
  (map!
   :map clojure-mode-map
   "C-M-q" #'sp-delete-sexp))
(define-key emacs-lisp-mode-map   (kbd "C-M-q") #'sp-delete-sexp)
(define-key lisp-interaction-mode-map (kbd "C-M-q") #'sp-delete-sexp)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; custom leader key bindings
(map!
 :leader
 (:prefix ("t" . "toggle")
  :desc "Toggle code folding" "o" #'origami-toggle-node
  :desc "Toggle all code folding" "O" #'avy-goto-char-2origami-toggle-all-nodes))

(defun god-mode-escape()
  "Escape god mode only if it is set."
  (interactive)
  (if god-local-mode
      (doom/escape)
    (god-local-mode)))

;; custom key bindings
(map!
 "<escape>" #'god-mode-escape
 "M-j" #'join-line
 "C-." #'embark-act
 :prefix "C-x"
 "C-1" #'delete-other-windows
 "C-2" #'split-window-below
 "C-3" #'split-window-right
 "C-0" #'delete-window)

(map!
 :prefix "C-c b"
 "n" #'next-buffer
 "p" #'previous-buffer)

(map!
 "C-M-g" #'avy-goto-char-2)

;; god mode key bindings
(map!
 :after god-mode
 :map god-local-mode-map
 "i" #'god-local-mode
 "z" #'repeat
 "[" #'backward-paragraph
 "]" #'forward-paragraph)

(map! :after cc-mode
      :map java-mode-map
      :localleader
      (:desc "run main class"           "r" #'dap-java-debug
       :desc "stop debug session"       "s" #'dap-delete-session))

;;; config.el ends here
