;; nate-programming.el
;; ui
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; code formatting
(use-package apheleia)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Git
(use-package magit)
;; forge for github stuff
(use-package forge
  :after magit)

;; git blame
(use-package blamer
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t))))

;; diff highlight marks on the gutter
(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; declutter emacs
(use-package perspective
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :hook ((kill-emacs-hook . persp-state-save))
  :init
  (persp-mode)
  :config
  (setq switch-to-prev-buffer-skip
        (lambda (win buff bury-or-kill)
          (not (persp-is-current-buffer buff)))))

;; expand region
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

;; enable hs-mode in prog-mode-hook
(defun nate/enable_hideshow()
  (interactive)
  (hs-minor-mode))
(add-hook 'prog-mode-hook #'nate/enable_hideshow)

(use-package rainbow-mode
  :hook org-mode prog-mode)

;; an awesome completion ranking
(use-package prescient)

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
(use-package eldoc
  :ensure nil
  :straight nil
  :config
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil))

(use-package eldoc-box
  :bind ("M-SPC h e" . eldoc-box-help-at-point)
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

;; folding backed by treesit
(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :config
  (global-treesit-fold-mode 1)
  (global-treesit-fold-indicators-mode 1)
  (treesit-fold-line-comment-mode 1)
  (setq treesit-fold-indicators-priority 100))

;; lsp
(use-package lsp-mode
  :hook ((c-ts-mode
          java-ts-mode
          go-ts-mode
          zig-mode
          js-ts-mode
          typescript-ts-mode
          web-mode
          astro-ts-mode
          tsx-ts-mode) . lsp-deferred)
  :custom
  (lsp-keymap-prefix "M-SPC l")
  (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  ;; ;; core
  (lsp-enable-xref t)
  ;; headerline
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  :init
  (setq lsp-idle-delay 0.500))

;; lsp booster copied from `https://github.com/blahgeek/emacs-lsp-booster'
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)
(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-global-modes '(not shell-mode eaf-mode))
  (company-idle-delay 0.1)
  :config
  (global-company-mode 1))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; combobulate treesitter based magic
(use-package combobulate
  :custom
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode))
  :load-path (lambda () (expand-file-name "combobulate.el" (straight--repos-dir "combobulate"))))

;;==== LANGUAGES =======;;
(global-subword-mode 1)
;; lisp
(use-package paredit
  :config
  :hook
  (lisp-mode . paredit-mode)
  (lisp-interaction-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode))

;; elisp
(use-package package-lint)
(use-package flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup))
(use-package eask-mode)

;; Clojure
(use-package clojure-ts-mode)
(use-package cider)
(use-package clojure-snippets)
(use-package clj-refactor
  :hook
  (clojure-mode . (lambda () (clr-add-keybindings-with-prefix "C-c C-m"))))

(setq openjdk-23-path "/home/nathan/.sdkman/candidates/java/17.0.12-graal/bin/java")
(use-package lsp-java
  :custom
  (lsp-java-java-path openjdk-23-path))

(use-package gradle-mode)
(use-package kotlin-ts-mode
  :mode "\\.kt\\'")

;; Zig
(use-package zig-mode)

;; markdown
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; LATEX
(use-package auctex)
(use-package latex-preview-pane)

;; docker and kube
(use-package docker
  :bind ("C-M-s-SPC o d" . docker))

(use-package kubernetes
  :bind ("C-M-s-SPC o k" . kubernetes-dispatch))

;; the web
(use-package web-mode
  :mode
  (("\\.mustache\\'" . web-mode)
   ("\\.xhtml\\'" . web-mode))
  :config
  (setq web-mode-enable-current-column-highlight t
        web-mode-enable-current-element-highlight t))

;; web assembly staff
(use-package wat-ts-mode
  :straight (:type git :host github :repo "nverno/wat-ts-mode")
  :mode (("\\.wat\\'" . wat-ts-mode)
         ("\\.wast\\'". wat-ts-wast-mode)))

;; Load custom jte mode
(use-package jte-mode
  :straight (:local-repo "~/Projects/emacs/jte-mode/" :host nil :type nil))

(use-package hyprlang-ts-mode
  :straight (:type git :host github :repo "Nathan-Melaku/hyprlang-ts-mode"))

(use-package lsp-tailwindcss
  :straight (:type git :host github :repo "merrickluo/lsp-tailwindcss")
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode)
         (css-ts-mode . emmet-mode)))

(use-package astro-ts-mode)
(use-package svelte-mode)

;; scala
(use-package scala-ts-mode
  :interpreter ("scala3" . scala-ts-mode))

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

;; C3
(use-package c3-ts-mode
  :straight (:type nil :host nil :local-repo "~/Projects/oss-contrib/c3-ts-mode/")
  :config
  (setq c3-ts-mode-indent-offset 4))

(provide 'nate-programming)
