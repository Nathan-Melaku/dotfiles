;; nate-programming.el
;; ui
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Git with Magic
(use-package magit)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; declutter emacs
(use-package perspective
  :init
  (persp-mode))

;; expand region
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; ts-fold
(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

;; project management
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Projects/")))

;; completion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.1))
        company-tooltip-align-annotations t
        company-transformers '(company-sort-by-backend-importance)))

;; completion ranking
(use-package prescient)
(use-package company-prescient
  :config
  (company-prescient-mode 1))

;; snippets
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'astro-ts-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets)

;; manage your parenthesis well
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; line numbers in programming modes
(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode)
                            (setq display-line-numbers 'relative)))

(add-hook 'astro-ts-mode-hook (lambda ()
                                (display-line-numbers-mode)
                                (setq display-line-numbers 'relative)))

(use-package eldoc-box
  :bind ("M-n h" . eldoc-box-help-at-point)
  :config
  (custom-set-faces
   '(eldoc-box-body
     ((t (:height 0.9 :family "Source Code Pro"))))))

;; treesitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; lsp
(use-package eglot
  :ensure nil
  :straight nil
  :config
  (setq eglot-extend-to-xref t)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-to-list 'company-backends
                           '(company-capf :with company-yasnippet))
              (add-to-list 'company-backends
                           '(company-capf :with company-files))))
  (add-to-list 'eglot-server-programs
               '(scala-ts-mode . ("metals" :initializationOptions
                                  (:sbtScript "/home/nathan/.local/share/coursier/bin/sbt"))))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(astro-ts-mode . ("astro-ls" "--stdio" :initializationOptions
                                  (:typescript (:tsdk "./node_modules/typescript/lib"))))))

(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'astro-ts-mode-hook 'eglot-ensure)
(add-hook 'svelte-mode-hook 'eglot-ensure)

;;==== LANGUAGES =======;;

;; markdown
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; the web
(use-package web-mode)
(use-package astro-ts-mode)
(use-package svelte-mode)

;; scala
(use-package scala-ts-mode
  :interpreter ("scala3" . scala-ts-mode)
  :hook (scala-ts-mode . eglot-ensure))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :mode ("\\.sbt\\'" . scala-ts-mode)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Go
(defun nate/go-revive-lint ()
  "Run revive for linting shows result in a new temp buffer"
  (interactive)
  (when buffer-file-name

    (let* ((file (file-relative-name buffer-file-name projectile-project-root))
           (output (shell-command-to-string (format "revive %s" (shell-quote-argument file)))))
      (with-output-to-temp-buffer "*Revive lint output*"
        (princ output)))))
;;(define-key evil-normal-state-map (kbd "SPC g r") 'nate/go-revive-lint)

(provide 'nate-programming)
