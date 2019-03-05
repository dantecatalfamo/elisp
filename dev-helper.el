;;; dev-helper -- heplers for managing dev project directories
;;; Commentary:
;;; A few helpful functions for managing and navigating work projects

;;; Code:

(require 'projectile)
(require 'helm)
(require 'treemacs)


(defun dev-add-project-projectile (project-root project-name)
  "Add project to projectile.
PROJECT-ROOT is the project's root directory, PROJECT-NAME is the name."
  (unless (member project-root
                  projectile-known-projects)
    (projectile-add-known-project project-root)))


(defun dev-add-project-treemacs (project-root project-name)
  "Add project to treemacs.
PROJECT-ROOT is the project's root directory, PROJECT-NAME is the name."
  (treemacs-do-add-project-to-workspace project-root
                                        project-name))

(defun dev-change-project (project-root)
  "Change dev projects, add the selected project to projectile and treemacs.
PROJECT-ROOT is the root directory of the project"
  (interactive (list
                (read-directory-name "Select Project Root: "
                                     "~/src/github.com/Shopify/")))
    (let ((project-name (file-name-nondirectory
                         (directory-file-name project-root))))

      (dev-add-project-projectile project-root project-name)
      (dev-add-project-treemacs project-root project-name))

    (let ((projectile-completion-system 'helm))
      (projectile-switch-project-by-name project-root)))

(provide 'dev-helper)
;;; dev-helper.el ends here
