(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

(package! origami)

(package! ts-fold
  :recipe (:host github :repo "emacs-tree-sitter/ts-fold" :files ("*.el" "dist")))
