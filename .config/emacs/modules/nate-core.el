;; nate-core.el

;; Disable the startup message
(setq gc-cons-threshold #x40000000
      read-process-output-max (* 1024 1024 4)
      inhibit-startup-message t
      byte-compile-warnings nil
      warning-minimum-level :emergency
      make-backup-files nil
      history-length 25
      use-dialog-box nil
      global-auto-revert-non-file-buffers t
      auth-sources '("~/.authinfo.gpg"))
(global-unset-key (kbd "M-SPC"))

;; custom vars on my location
(setq custom-file (locate-user-emacs-file "nate-custom.el"))
(load custom-file 'noerror 'nomessage)

;; Enable vertico
(use-package vertico
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode 1))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  ;; history
  (recentf-mode 1)
  (save-place-mode 1)
  (global-auto-revert-mode 1)
  (file-name-shadow-mode 1)
  (column-number-mode t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (delete-selection-mode 1)
  (global-auto-revert-non-file-buffers t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (ispell-dictionary "en_US")
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (split-width-threshold 300)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (indent-tabs-mode nil)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  ;; compilation mode tweeks
  (compilation-scroll-ouptput t)
  (compilation-auto-jump-to-first-error t)
  (compilation-max-output-line-length nil)
  :config
  (setopt compilation-ask-about-save nil)
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)
  (winner-mode 1)
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :bind
  ("C-x C-b" . ibuffer))

(use-package window
  :ensure nil
  :straight nil
  :custom
  (setq display-buffer-alist
        '(
          ("\\*.*e?shell\\*"
           (display-buffer-in-side-window)
           (window-hight . 0.25)
           (side . bottom)
           (slot . -1))

          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
           (display-buffer-in-side-window)
           (window-hight . 0.25)
           (side . bottom)

           ("\\*\\(lsp-help\\)\\*"
            (display-buffer-in-side-window)
            (window-height . 0.25)
            (side . bottom)
            (slot . 0))
           ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
            (display-buffer-in-side-window)
            (window-height . 0.25)
            (side . bottom)
            (slot . 1))  (slot . 0))
          )))

;; display whitespaces properly
(use-package whitespace
  :ensure nil
  :straight nil
  :config
  (setq
   whitespace-style '(face tabs tab-mark spaces space-mark trailing newline newline-mark)
   whitespace-display-mappings '(;;(space-mark   ?\     [?\u00B7]     [?.])
                                 (space-mark   ?\xA0  [?\u00A4]     [?_])
                                 (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-global-modes '(prog-mode))
  (global-whitespace-mode 1))

;; rg for searching
(use-package rg)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-," . embark-dwim)
   ("C-h B" . embark-bindings)))
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package corfu
;;   :ensure t
;;   :hook (after-init . global-corfu-mode)
;;   :bind (:map corfu-map ("<tab>" . corfu-complete))
;;   :config
;;   (setq tab-always-indent 'complete)
;;   (setq corfu-preview-current nil)
;;   (setq corfu-min-width 20)
;;   (setq corfu-popupinfo-delay '(1.25 . 0.5))
;;   (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'
;;   (global-set-key (kbd "C-M-s-c")(lambda ()
;;                                    (interactive)
;;                                    (if corfu-auto
;;                                        (setq corfu-auto nil)
;;                                      (setq corfu-auto t))
;;                                    (corfu-mode -1)
;;                                    (corfu-mode 1)))
;;   ;; Sort by input history (no need to modify `corfu-sort-function').
;;   (with-eval-after-load 'savehist
;;     (corfu-history-mode 1)
;;     (add-to-list 'savehist-additional-variables 'corfu-history)))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` "))

;; Add extensions for corfu
;; (use-package cape
;;   :bind ("M-p" . cape-prefix-map)
;;   :init
;;   (defun cape-dabbrev-dict-keyword ()
;;     (cape-wrap-super #'cape-dabbrev #'cape-dict #'cape-keyword))
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev-dict-keyword)
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-elisp-block)
;;   (add-hook 'completion-at-point-functions #'cape-history))

(repeat-mode 1)
(provide 'nate-core)
