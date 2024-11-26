;; nate-keybindings.el
;; scroll
(global-set-key (kbd "C-M-s-n") #'pixel-scroll-up)
(global-set-key (kbd "C-M-s-p") #'pixel-scroll-down)

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

    ;; buffer related binding
    "b" '(:ignore t :which-key "buffer")
    "b k" 'kill-buffer
    "b b" 'consult-buffer
    "b s" 'save-buffer

    ;; files
    "f" '(:ignore t :which-key "files")
    "f r" '(recentf :which-key "find recent files")

    ;; Open
    "o" '(:ignore t :which-key "open")
    "o t" '(vterm-other-window :which-key "open terminal below")

    ;; Org-roam bindigs
    "n" '(:ignore t :which-key "notes")
    "n f" 'org-roam-node-find
    "n i" 'org-roam-node-insert
    "n c" 'org-roam-capture

    ;; toggle
    "t" '(:ignore t :which-key "toggle")
    "t f" '(treesit-fold-toggle :which-key "toggle folding")

    ;; version control
    "v" '(:ignore t :which-key "version control")
    "v v" '(magit :which-key "magit dispatch")
    "v b" '(blamer-show-posframe-commit-info :which-key "git blame")

    "w" '(:ignore t :which-key "window management")
    "w b" '(winner-undo :which-key "winner undo")
    "w f" '(winner-redo :which-key "winner redo")
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
