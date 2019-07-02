(defmacro deftag (name)
  (let* ((tagname (symbol-name name))
         (tagtext (format "<%s%%s>%%s</%s>" tagname tagname)))
    `(defmacro ,name (tags &rest inner)
         `(format ,,tagtext
                  (if (listp ,tags)
                      (apply #'concat (mapcar #'eval ,tags))
                    ,tags)
                  (apply #'concat (mapcar #'eval ',inner))))))

(defmacro deftag-short (name)
  (let* ((tagname (symbol-name name))
         (shorttag (intern (concat tagname "1"))))
    `(defmacro ,shorttag (&rest inner)
       `(,',name 'nil ,@inner))))

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

(defmacro defattr (name)
  (let* ((attrname (symbol-name name))
         (attrtext (format " %s=\"%%s\"" attrname)))
    `(defun ,name (&rest values)
       (format ,attrtext (mapconcat #'identity values " ")))))

(defun attr (attr &rest values)
  (format " %s=\"%s\"" attr (mapconcat #'identity values " ")))

(attr 'id "one")

(defmacro defattrs (&rest attrs)
  (let (defined)
    (dolist (attr attrs defined)
      (push `(defattr ,attr) defined))
    `(progn ,@defined)))

(deftags1 html head title body div p b script style link span)
(defattrs class id)
