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

  ;; buffer related binding
  "b k" 'kill-buffer
  "b b" 'consult-buffer
  "b s" 'save-buffer
  )

(with-eval-after-load 'eglot
  (nate-leader-map
	"l f" 'eglot-format
	"l r" 'eglot-rename
	"l a" 'eglot-code-actions
	"l o" 'eglot-code-action-organize-imports))

(provide 'nate-keybindings)
