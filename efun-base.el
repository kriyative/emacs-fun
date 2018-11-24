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

(provide 'efun-base)
