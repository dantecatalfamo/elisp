;;; dev -- heplers for managing dev project directories
;;; Commentary:
;;; A few helpful functions for managing and navigating work projects

;;; Code:

(require 'projectile)
(require 'helm)
(require 'treemacs)

(defun dev-change-project (project-root)
  "Change dev projects, add the selected project to projectile and treemacs.
PROJECT-ROOT is the root directory of the project"
  (interactive (list
                (read-directory-name "Select Project Root: "
                                     "~/src/github.com/Shopify/")))
    (let ((project-name (file-name-nondirectory
                         (directory-file-name project-root))))
      (unless (member project-root projectile-known-projects)
        (projectile-add-known-project project-root))
      (treemacs-do-add-project-to-workspace project-root project-name)
      (cd project-root)
      (helm-projectile)))

(provide 'dev)
;;; dev.el ends here
