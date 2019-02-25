;;; Org-TV --- Easily add TV shows to Org-Mode file
;;; Commentary:
;;; Adds a TV show as an org heading, with each season as a
;;; subheading, and each episode as a checkbox

;;; Code:
(defun org-tv-new (name seasons)
  "Create Org-mode list for TV show.
NAME is show, SEASONS is number of seasons."
  (interactive "sShow Name: \nnNumber of seasons: ")

  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))

  (if (eq seasons 1)
      (my-tv-new-season name t)

    (org-insert-subheading t)
    (insert name)
    (dotimes (i seasons)
      (my-tv-new-season (concat "Season " (number-to-string (+ 1 i)))
                        (eq i 0)))))

(defun org-tv-new-season (name &optional indent)
  "Insert season, part of tv thing.
NAME is name of the season, INDENT is weather or not to indent further."
  (if indent
      (org-insert-subheading t)
    (org-insert-heading t))
  (insert name)
  (let ((episodes (read-number (concat "Episodes in " name ": "))))
    (org-return-indent)
    (insert "- [ ] Ep. 1")
    (dotimes (e (- episodes 1))
      (org-insert-todo-heading t)
      (insert (concat "Ep. " (number-to-string (+ 2 e)))))))

(provide 'org-tv-new)
;;; org-tv.el ends here
