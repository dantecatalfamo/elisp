(defmacro deftag (name)
  (let* ((tagname (symbol-name name))
         (tagtext (format "<%s%%s>%%s</%s>" tagname tagname)))
    `(defmacro ,name (tags &rest inner)
       `(format ,,tagtext
                (attrs ',tags)
                (apply #'concat (mapcar #'eval ',inner))))))

(defmacro deftag-short (name)
  (let* ((tagname (symbol-name name))
         (shorttag (intern (concat tagname "1"))))
    `(defmacro ,shorttag (&rest inner)
       `(,',name nil ,@inner))))

(defmacro deftags (&rest tags)
  (let (defined)
    (dolist (tag tags defined)
      (push `(deftag ,tag) defined))
    `(progn ,@defined)))

(defmacro deftags-short (&rest tags)
  (let (defined)
    (dolist (tag tags defined)
      (push `(deftag1 ,tag) defined))
    `(progn ,@defined)))

(defmacro deftags1 (&rest tags)
  `(progn (deftags ,@tags)
          (deftags-short ,@tags)))

(defun attrs (attrs)
  (cond
   ((listp (car attrs))
    (apply #'concat (mapcar #'attrs attrs)))
   (attrs (format " %s=\"%s\"" (car attrs) (mapconcat #'identity (cdr attrs) " ")))
   (t nil)))

(deftags1 html head title body div p b script style link span)
