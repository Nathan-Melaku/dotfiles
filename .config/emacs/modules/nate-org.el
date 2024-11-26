;; nate-org.el
(use-package verb)

(use-package org
  :ensure nil
  :straight nil
  :config
  (setq org-directory "~/Documents/Org")
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :config
  (org-superstar-configure-like-org-bullets))

(use-package org-roam
  :config
  (setq org-roam-directory "~/Documents/Org/roam"))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(provide 'nate-org)
