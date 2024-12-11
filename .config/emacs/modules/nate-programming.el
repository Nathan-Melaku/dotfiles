;; nate-programming.el
;; ui
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; code formatting
(use-package apheleia
  :config
  (apheleia-global-mode t))

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

;; project management
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Projects/"
                                         "~/Projects/oss-contrib/"
                                         "~/Projects/tryouts/zig/"
                                         "~/Projects/emacs/")))

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
(use-package eglot
  :ensure nil
  :straight nil
  :config
  (setq eglot-extend-to-xref t)
  (add-to-list 'eglot-server-programs
               '(scala-ts-mode . ("metals" :initializationOptions
                                  (:sbtScript "/home/nathan/.local/share/coursier/bin/sbt"))))
  (add-to-list 'eglot-server-programs
               '(java-ts-mode . ("jdtls")))
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(astro-ts-mode . ("astro-ls" "--stdio" :initializationOptions
                                  (:typescript (:tsdk "./node_modules/typescript/lib"))))))

(use-package eglot-booster
  :straight `(:type git :host github :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode))

(use-package sideline
  :hook ((flymake-mode . sideline-mode))
  :init
  (use-package sideline-flymake
    :init
    (setq sideline-flymake-display-mode 'line
          sideline-backends-right '(sideline-flymake)))
  (setq sideline-backends-left-skip-current-line t
        sideline-order-left 'down
        sideline-order-right 'up
        sideline-format-left "%s   "
        sideline-format-right "   %s"
        sideline-priority 100))

(use-package dape
  :preface
  (setq dape-key-prefix "\M-n\M-d")
  :config
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-inlay-hint t))

(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'astro-ts-mode-hook 'eglot-ensure)
(add-hook 'svelte-mode-hook 'eglot-ensure)
(add-hook 'zig-mode-hook 'eglot-ensure)

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
(use-package package-lint-flymake)
(use-package eask-mode)

;; Clojure
(use-package clojure-ts-mode)
(use-package cider)
(use-package clojure-snippets)
(use-package clj-refactor
  :hook
  (clojure-mode . (lambda () (clr-add-keybindings-with-prefix "C-c C-m"))))

;; java
(use-package eglot-java
  :hook (java-ts-mode . eglot-java-mode)
  :config
  (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
  (defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
    "Custom options that will be merged with any default settings."
    '(:bundles: ["/home/nathan/Tools/java-debug-0.53.1/com.microsoft.java.debug.plugin/target/com.microsoft.java.debug.plugin-0.53.1.jar"]
                :settings: (:java (:format (:settings
                                            (:url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml") :enabled t)))
                :extendedClientCapabilities (:classFileContentsSupport t))))


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

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode)
         (css-ts-mode . emmet-mode)))

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
