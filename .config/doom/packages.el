;;; packages.el -*- lexical-binding: t; -*-
(package! origami)
(package! spacious-padding)
(package! modus-themes)
(package! ng2-mode)
(package! eat
  :recipe (:type git
           :host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))
(package! golden-ratio)
(package! evil-tutor)
(package! undo-tree)
(package! beacon)
