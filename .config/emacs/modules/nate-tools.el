;; nate-tools.el
;; eval inline
(use-package eros
  :straight t
  :config
  (eros-mode 1))

(use-package vterm)
(use-package ace-window
  :bind ("C-x o" . 'ace-window))

;; Dired mode 
(use-package dired+
  :config
  (setq dired-listing-switches "-alh -v --group-directories-first")
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

(provide 'nate-tools)
