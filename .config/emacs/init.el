;; init.el
;; bootstrap striaght
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(put 'upcase-region 'disabled nil)

;; Load custom modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'nate-core)
(require 'nate-ui)
;;(require 'nate-evil)
(require 'nate-programming)
(require 'nate-tools)
(require 'nate-org)
(require 'nate-eshell)
(require 'nate-keybindings)

(provide 'init.el)
