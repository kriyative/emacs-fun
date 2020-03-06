;;; efun-base.el --- basic elisp functions and macros

(defun region-string ()
  "Return the contents of the current region as a string."
  (when mark-active
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun mklist (x)
  "Ensure X is a list."
  (if (listp x)
      x
    (list x)))

(defun partition (list len)
  "Partition LIST into sublists of length LEN. e.g.,

  (partition '(1 2 3 4 5) 2)
  => '((1 2) (3 4) (5 nil))"
  (do ((list list (nthcdr len list))
       (result nil (cons (subseq list 0 len) result)))
      ((null list) (reverse result))))

(defun join (sep list)
  "Concatenate LIST of strings into a string separated by SEP. e.g,

  (join \",\" '(\"foo\" \"bar\"))
  => \"foo,bar\""
  (mapconcat 'identity list sep))

(defun str (x)
  "Return the string form of X"
  (etypecase x
    (symbol (subseq (symbol-name x) (if (keywordp x) 1 0)))
    (string x)
    (t (prin1-to-string x))))

(defmacro bind (clauses &rest body)
  "This macro combines the behaviour of the forms `let*',
`destructuring-bind', and `multiple-value-bind', permitting the
following style of binding form:

  (bind (((:values m n) (values 10 20))
         ((a b _c &key (d 10)) '(1 2 3))
         (x 5))
    (+ x a b d m n))
  => 48

Note in the destructuring form (a b _c &key (d 10)), _c is a short form
for declaring it as ignorable.

This is a more limited and lightweight implementation of some ideas from
metabang-bind (http://common-lisp.net/project/metabang-bind/)."
  (cl-labels
      ((parse-arglist (args)
         (loop
            for arg in args
            collect arg into args
            when (and (symbolp arg) (eq (aref (symbol-name arg) 0) ?_))
            collect arg into ignorables
            finally (return (values args ignorables))))
       (cons-form (form args clauses body)
         (multiple-value-bind (arglist ignorables)
             (parse-arglist args)
           `(,form ,arglist
                   ,@(cdar clauses)
                   ,@(when ignorables `((declare ,(list* 'ignore ignorables))))
                   (bind ,(cdr clauses) ,@body)))))
    (cond
      ((null clauses) `(progn ,@body))
      ((sequencep (caar clauses))
       (cond
         ((eq (caaar clauses) :values)
          (cons-form 'multiple-value-bind (cdaar clauses) clauses body))
         ((eq (caaar clauses) :slots)
          `(with-slots ,(cdaar clauses) ,@(cdar clauses)
             (bind ,(cdr clauses) ,@body)))
         (t
          (cons-form 'destructuring-bind (caar clauses) clauses body))))
      (t
       `(let (,(car clauses))
          (bind ,(cdr clauses) ,@body))))))

(defmacro with-cwd (dir &rest body)
  "Set the current directory to DIR, invoke BODY and restore
working directory back to its original value."
  (let ((cwd% (gensym)))
    `(let ((,cwd% default-directory))
       (unwind-protect
           (progn
             (cd-absolute ,dir)
             ,@body)
         (cd-absolute ,cwd%)))))

(defmacro save-values (vars &rest body)
  "Save the values of VARS, invoke BODY and restore the original
VARS values."
  (let ((tmpargs (mapcar (lambda (x) (gensym x)) vars)))
    `(let ,(map 'list
                (lambda (tmparg var)
                  (list tmparg var))
                tmpargs
                vars)
       (unwind-protect
           (progn ,@body)
         ,@(map 'list
                (lambda (var tmparg)
                  (list 'setq var tmparg))
                vars
                tmpargs)))))

(defun try-require (feature)
  "A more forgiving `require`."
  (condition-case nil
      (require feature)
    (error (message "Error loading feature %s" feature))))

(defmacro if-bind (binding then &optional else)
  "Yet another implementation of IF form, which binds result of
the predicate in the scope of the THEN form, e.g.,

  (if-bind (existsp (file-exists-p \"/tmp/dummy\"))
    (message \"does the file exist? %s\" existsp)
    (message \"no joy\"))"
  (declare (indent 1))
  (destructuring-bind (var predicate)
      binding
    `(let ((,var ,predicate))
       (if ,var
           ,then
         ,else))))

(defmacro when-bind (binding &rest body)
  "Yet another implementation of WHEN form, which binds result of
the predicate in the scope of the BODY form, e.g.,

  (when-bind (existsp (file-exists-p \"/tmp/dummy\"))
    (message \"does the file exist? %s\" existsp))"
  (declare (indent 1))
  `(if-bind ,binding
     (progn
       ,@body)))

(defun locate-path (file path-list)
  "Searches for specified file in PATH-LIST, returns full
pathname if found."
  (cl-labels ((concat-path (path file)
                           (concat (file-name-as-directory path) file)))
    (when-bind (path (find-if (lambda (path)
                                (let ((file-path (concat-path path file)))
                                  (and (file-exists-p file-path) file-path)))
                              path-list))
      (concat-path path file))))

(defun re-matches (re str)
  "Return a list of matched substrings of RE in STR."
  (when (string-match re str)
    (mapcar (lambda (match)
              (apply 'substring str match))
            (partition (match-data) 2))))

(defun html (spec)
  "Generate a string representation of the specified HTML spec."
  (labels ((spaced (seq) (join " " seq))
           (attr-str (attrs)
             (spaced (mapcar (lambda (x)
                               (destructuring-bind (key val) x
                                 (concat (str key) "=\"" (str val) "\"")))
                             (partition attrs 2))))
           (seq (x)
             (if (listp x) x (list x))))
    (if (listp spec)
        (let ((head (first spec)))
          (destructuring-bind (tag &rest attribs) (seq head)
            (join ""
                  (append
                   (list*
                    (concat "<" (str tag)
                            (if (zerop (length attribs))
                                ""
                                (concat " " (attr-str attribs)))
                            ">")
                    (mapcar 'html (rest spec)))
                   (list (concat "</" (str tag) ">"))))))
      spec)))

;;; https://www.emacswiki.org/emacs/AddCommasToNumbers
(defun add-number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string. Optional
SEPARATOR is the string to use to separate groups. It defaults to
a comma."
  (when number
    (let ((num (if (stringp number)
                   number
                 (number-to-string number)))
          (op (or separator ",")))
      (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
        (setq num (concat
                   (match-string 1 num) op
                   (match-string 2 num))))
      num)))

(provide 'efun-base)
