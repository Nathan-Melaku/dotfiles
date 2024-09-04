;; nate-programming.el
;; Git with Magic
(use-package magit)

;; declutter emacs
(use-package perspective
  :after evil
  :init
  (persp-mode))

;; project management 
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Projects/")))

;; completion
(use-package company
  :hook (after-init . global-company-mode))

;; snippets
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

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
;; lsp
(use-package eglot
  :ensure nil
  :straight nil
  :config
  (setq eglot-extend-to-xref t)
  (add-hook 'eglot-managed-mode-hook
			(lambda ()
              (add-to-list 'company-backends
						   '(company-capf :with company-yasnippet))))
  (add-to-list 'eglot-server-programs
			   '(astro-ts-mode . ("astro-ls" "--stdio"
							   :initializationOptions
							   (:typescript (:tsdk "./node_modules/typescript/lib"))))))
;; the web
(use-package web-mode)
(use-package astro-ts-mode)

;; treesitter
(setq treesit-language-source-alist
	  '((astro "https://github.com/virchau13/tree-sitter-astro")
		(bash "https://github.com/tree-sitter/tree-sitter-bash")
		(cmake "https://github.com/uyha/tree-sitter-cmake")
		(css "https://github.com/tree-sitter/tree-sitter-css")
		(elisp "https://github.com/Wilfred/tree-sitter-elisp")
		(go "https://github.com/tree-sitter/tree-sitter-go")
		(html "https://github.com/tree-sitter/tree-sitter-html")
		(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
		(json "https://github.com/tree-sitter/tree-sitter-json")
		(make "https://github.com/alemuller/tree-sitter-make")
		(markdown "https://github.com/ikatyang/tree-sitter-markdown")
		(python "https://github.com/tree-sitter/tree-sitter-python")
		(toml "https://github.com/tree-sitter/tree-sitter-toml")
		(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
		(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun nate/install-treesit-grammers ()
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

;; mapping to ts modes
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))

;; go
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'astro-ts-mode-hook 'eglot-ensure)

(defun nate/go-revive-lint ()
  "Run revive for linting shows result in a new temp buffer"
  (interactive)
  (when buffer-file-name

	(let* ((file (file-relative-name buffer-file-name projectile-project-root))
		   (output (shell-command-to-string (format "revive %s" (shell-quote-argument file)))))
	  (with-output-to-temp-buffer "*Revive lint output*"
		(princ output)))))
(define-key evil-normal-state-map (kbd "SPC g r") 'nate/go-revive-lint)

(provide 'nate-programming)
