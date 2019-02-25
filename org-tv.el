;;; Org-TV --- Easily add TV shows to Org-Mode file
;;; Commentary:
;;; Adds a TV show as an org heading, with each season as a
;;; subheading, and each episode as a checkbox

;;; Code:
(require 'org)

(defun org-tv-new (name seasons todo)
  "Create Org-mode list for TV show.
NAME is show, SEASONS is number of seasons, TODO is weather or not to
make it into a todo entry with a percentage for each season"
  (interactive (list (read-string "Show Name: ")
                     (read-number "Number of seasons: ")
                     (y-or-n-p "TODO entry? ")))

  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))

  (if (eq seasons 1)
      (org-tv-new-season name t todo)

    (if todo
        (org-insert-todo-subheading t)
      (org-insert-subheading t))
    (insert name)
    (when todo
      (insert " [/]")
      (org-update-statistics-cookies nil))
    (dotimes (i seasons)
      (org-tv-new-season (concat "Season " (number-to-string (+ 1 i)))
                        (eq i 0)
                        todo))))

(defun org-tv-new-season (name &optional indent todo)
  "Insert season, part of org-tv.
NAME is name of the season, INDENT is weather or not to indent
further, TODO adds a percentage to the end of the season line"
  (if indent
      (if todo
          (org-insert-todo-subheading t)
        (org-insert-subheading t))
    (if todo
        (org-insert-todo-heading-respect-content t)
      (org-insert-heading t)))
  (insert name)
  (when todo
    (insert " [%]")
    (org-update-statistics-cookies nil))
  (let ((episodes (read-number (concat "Episodes in " name ": "))))
    (org-return-indent)
    (insert "- [ ] Ep. 1")
    (org-update-statistics-cookies nil)
    (dotimes (e (- episodes 1))
      (org-insert-todo-heading t)
      (insert (concat "Ep. " (number-to-string (+ 2 e)))))))

(provide 'org-tv)
;;; org-tv.el ends here
