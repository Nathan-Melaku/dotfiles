;; nate-keybindings.el

(use-package general
  :config 
  (general-define-key
   :states '(emacs insert normal)
   :prefix-map 'nate-prefix-map
   :global-prefix "C-c"
   :non-normal-prefix "M-SPC"
   :prefix "SPC")
  (general-create-definer nate-leader-map
	:keymaps 'nate-prefix-map))

(nate-leader-map
  ;; generic keybindings
  ";"   'execute-extended-command 
  "SPC" 'projectile-find-file
  "."   'find-file
  ","   'persp-switch-to-buffer
  "TAB" 'perspective-map
  "p"   'projectile-command-map

  ;; files
  "r f" 'recentf

  ;; search
  "s" 'avy-goto-char-timer

  ;; buffer related binding
  "b k" 'kill-buffer
  "b b" 'consult-buffer
  "b s" 'save-buffer

  ;; Org-roam bindigs
  "n f" 'org-roam-node-find
  "n i" 'org-roam-node-insert
  "n c" 'org-roam-capture
  )

;; Eglot mode binding
(with-eval-after-load 'eglot
  (nate-leader-map
	"l f" 'eglot-format
	"l r" 'eglot-rename
	"l a" 'eglot-code-actions
	"l o" 'eglot-code-action-organize-imports))

;; Dired mode bindings
(with-eval-after-load 'dired
  (general-define-key
   :states '(normal emacs)
   :keymaps 'dired-mode-map
   "RET" 'dired-single-buffer
   "^" 'dired-single-up-directory
   "l" 'dired-single-buffer
   "h" 'dired-single-up-directory))

(provide 'nate-keybindings)
