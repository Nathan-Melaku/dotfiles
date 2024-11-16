;; nate-keybindings.el

(defun nate/spawn-eat ()
  "open an eat shell below the given window"
  (interactive)
  (split-window-below)
  (other-window 1)
  (eat))


(use-package general
  :config
  (defconst nate-leader "M-SPC")
  (general-create-definer nate-leader-map
    :prefix nate-leader)
  (nate-leader-map
  ;; generic keybindings
  ";"   'execute-extended-command
  "SPC" 'projectile-find-file
  "."   'find-file
  ","   'persp-switch-to-buffer
  "TAB" 'perspective-map
  "p"   'projectile-command-map
  "d"   'duplicate-line
  "y"   'copy-from-above-command
  "c"   'compile

  ;; files
  "f" '(:ignore t :which-key "files")
  "f r" '(recentf :which-key "find recent files")

  ;; search
  "s" 'avy-goto-char-timer

  ;; toggle
  "t" '(:ignore t :which-key "toggle")
  "t f" '(treesit-fold-toggle :which-key "toggle folding")

  ;; Open
  "o" '(:ignore t :which-key "open")
  "o t" '(nate/spawn-eat :which-key "open terminal below")

  ;; buffer related binding
  "b" '(:ignore t :which-key "buffer")
  "b k" 'kill-buffer
  "b b" 'consult-buffer
  "b s" 'save-buffer

  ;; Org-roam bindigs
  "n" '(:ignore t :which-key "notes")
  "n f" 'org-roam-node-find
  "n i" 'org-roam-node-insert
  "n c" 'org-roam-capture
  )

;; Eglot mode binding
(with-eval-after-load 'eglot
  (nate-leader-map
    "l" '(:ignore t :which-key "lsp")
    "l f" 'eglot-format
    "l r" 'eglot-rename
    "l a" 'eglot-code-actions
    "l o" 'eglot-code-action-organize-imports)))

;; Dired mode bindings
(with-eval-after-load 'dired
  (general-define-key
   :keymaps 'dired-mode-map
   "RET" 'dired-single-buffer
   "^" 'dired-single-up-directory
   "l" 'dired-single-buffer
   "h" 'dired-single-up-directory))

(provide 'nate-keybindings)
