;; nate-tools.el
(use-package undo-tree
  :config
  (undo-tree-mode 1)
  :bind
  ("C-M-s-?" . undo-tree-undo)
  ("C-M-s-|" . undo-tree-redo))

;; help menu
(use-package which-key
  :config
  (which-key-mode))

;; eval inline
(use-package eros
  :straight t
  :config
  (eros-mode 1))

(use-package ws-butler
  :config
  (ws-butler-global-mode 1))

(use-package avy
  :straight nil
  :ensure nil
  :config
  (setq avy-timeout-seconds 0.2)
  :bind
  ("C-;" . avy-goto-char-timer)
  ("C-:" . avy-goto-char-2)
  ("C-M-s-f" . avy-goto-char-in-line))

(use-package ace-link
  :bind
  (:map org-mode-map
        ("C-M-s-o" . ace-link-org))
  (:map Info-mode-map
        ("C-M-s-o" . ace-link-info))
  (:map help-mode-map
        ("C-M-s-o" . ace-link-help))
  (:map compilation-mode-map
        ("C-M-s-o" . ace-link-compilation))
  (:map eww-mode-map
        ("C-M-s-o" . ace-link-eww)))

(use-package zzz-to-char
  :bind ("M-z" . zzz-to-char))

;; Testing fzf and emacs
(use-package fzf
  :bind
  ("C-M-s-SPC f" . fzf-find-file)
  ("C-M-s-SPC p" . fzf-projectile)
  ("C-M-s-SPC j" . fzf-grep)
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package vterm)
(use-package eat
  :straight '(eat :type git
                  :host codeberg
                  :repo "akib/emacs-eat"
                  :files ("*.el" ("term" "term/*.el") "*.texi"
                          "*.ti" ("terminfo/e" "terminfo/e/*")
                          ("terminfo/65" "terminfo/65/*")
                          ("integration" "integration/*")
                          (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("M-o" . 'ace-window))

(defun nate/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

;; Dired mode
(use-package dired
  :ensure nil
  :straight nil
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ( "."     . dired-omit-mode))
  :custom
  (dired-omit-files "\\`[.].*")
  (dired-listing-switches "-alh -v --group-directories-first"))

(use-package dired+
  :config
  (require 'dired+))

(use-package dired-single)

(defun nate/dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (nate/dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'nate/dired-init))

(use-package dired-preview
  :config
  (setq dired-preview-delay 0.5)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "epub"
                "\\)"))
  ;; (dired-preview-global-mode 1)
  )

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package pdf-tools)
;; password manager
(use-package pass)

;; treemacs
(use-package treemacs
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit)

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

;; moveing text up and down with M-Up and M-Down
(use-package move-text
  :config
  (move-text-default-bindings))
;; indent after move
(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)

(use-package bm
  :demand t
  :init
  (setq bm-restore-repository-on-load t)
  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)
  (setq bm-repository-file "~/.config/emacs/bm-repository")
  (setq-default bm-buffer-persistence t)
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (setq bm-marker 'bm-marker-left)
  :bind
  (("C-M-s-b n" . bm-next)
   ("C-M-s-b p" . bm-previous)
   ("C-M-s-b b" . bm-toggle)))

;; define a hydra for moving text
(use-package hydra)

(defhydra nate-hydra/move-text (global-map "M-SPC m")
  "move region/line using a hydra.

The heads for the associated hydra are:

\"n\": `move region down'
\"p\": `move region up'
\"N\": `move line down'
\"P\": `move line up'

This can be accessed via nate-hydra/move-text, which is bound to \"M-SPC m\" "
  ("p" move-text-region-up)
  ("n" move-text-region-down)
  ("P" move-text-up)
  ("N" move-text-down))

(use-package elfeed
  :config
  (setq elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        "http://www.50ply.com/atom.xml"  ; no autotagging
        ("http://nedroid.com/feed/" webcomic)
        "http://endlessparentheses.com/atom.xml"
        "https://ziglang.org/news/index.xml"
        )))
(provide 'nate-tools)
