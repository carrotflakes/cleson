(in-package :cl-user)

(defpackage :cleson
  (:use :cl)
  (:export :undefined-pattern
           :undefined-pattern-function
           :failed-pattern-match
           :define-pattern
           :define-pattern-function
           :match
           :match-all
           :match-lambda
           :match-all-lambda)
  (:import-from :alexandria
                :with-gensyms))

(in-package :cleson)


;;; utility macros
(defmacro eval-once (form)
  (let ((symbol (gensym)))
    `(progn (defvar ,symbol ,form) (symbol-value ',symbol))))


;;; conditions
(define-condition undefined-pattern (error)
  ((constructor :initarg :constructor))
  (:report
   (lambda (condition stream)
     (format stream "The pattern constructor ~s is undefined"
             (slot-value condition 'constructor)))))

(define-condition undefined-pattern-function (error)
  ((function-name :initarg :function-name))
  (:report
   (lambda (condition stream)
     (format stream "The pattern function ~s is undefined"
             (slot-value condition 'function-name)))))

(define-condition failed-pattern-match (error)
  ())


;;; structures
(defstruct state
  (bindings '())
  pairs)


;;; stores
(defvar *patterns-map* (make-hash-table :test 'eq))


;;; generics
(defgeneric pattern-function-compile (symbol args)
  (:method (symbol args)
    (error 'undefined-pattern-function :function-name symbol)))


;;; pettern define macros
(defmacro define-pattern (name lambda-list target &body body)
  `(setf (gethash ,name *patterns-map*)
         (lambda (,target ,@lambda-list) ,@body)))

(defmacro define-pattern-function (name args pattern-form)
  `(defmethod pattern-function-compile ((symbol (eql ',name)) (prms list))
     (let ((pattern ',pattern-form))
       (loop
          for arg in ',args
          for prm in prms
          do (setf pattern (subst prm arg pattern)))
       (pattern-compile pattern))))


;;; auxiliary functions
(defun remove-prefix (pattern-variables)
  (intern (subseq (symbol-name pattern-variables) 1)
          (symbol-package pattern-variables)))

(defun lambda-compile (form)
  (handler-bind ((warning #'muffle-warning))
    (coerce `(lambda () ,form) 'function)))

(defun eval-in (thunk bindings)
  "Evaluate thunk in the environment."
  (handler-bind ((warning #'muffle-warning))
    (progv
        (nreverse (mapcar #'car bindings))
        (nreverse (mapcar #'cdr bindings))
      (funcall thunk))))

(defun funcall-in (func arg bindings)
  "Call func in the environment."
  (handler-bind ((warning #'muffle-warning))
    (progv
        (nreverse (mapcar #'car bindings))
        (nreverse (mapcar #'cdr bindings))
      (funcall func arg))))

(defun apply-pattern (constructor args target)
  (apply (or (gethash constructor *patterns-map*)
             (error 'undefined-pattern :constructor constructor))
         target args))


;;; cardinal functions
(defun pattern-compile (pattern)
  (etypecase pattern
    (symbol
     (cond
       ((string= pattern "_") '(wildcard))
       ((string= pattern "$" :start1 0 :end1 1)
        `(pattern-variables ,(remove-prefix pattern)))
       ((string= pattern "=" :start1 0 :end1 1)
        `(equal-variable ,(remove-prefix pattern)))
       (t pattern)))
    (list
     (cond
       ((string= (first pattern) "?")
        `(? ,(eval (second pattern))))
       ((string= (first pattern) "=")
        `(equalp ,(lambda-compile (second pattern))))
       ((string= (first pattern) "EQ")
        `(eq ,(lambda-compile (second pattern))))
       ((string= (first pattern) "EQL")
        `(eql ,(lambda-compile (second pattern))))
       ((string= (first pattern) "EQUAL")
        `(equal ,(lambda-compile (second pattern))))
       ((string= (first pattern) "EQUALP")
        `(equalp ,(lambda-compile (second pattern))))
       ((string= (first pattern) "LET")
        `(let
           ,(loop
               for (id form) in (second pattern)
               collect (cons id (lambda-compile form)))
           ,(pattern-compile (third pattern))))
       ((string= (first pattern) "^")
        `(^ ,(pattern-compile (second pattern))))
       ((member (first pattern) '(* +) :test #'string=)
        `(,(first pattern) ,@(mapcar #'pattern-compile (cdr pattern))))
       (t (handler-case (pattern-function-compile (first pattern) (cdr pattern))
            (undefined-pattern-function ()
              `(,(first pattern) ,@(mapcar #'pattern-compile (cdr pattern))))))))))

(defun next-states (state)
  (with-slots (bindings pairs)
      state

    (when (null pairs)
      (return-from next-states '()))

    (destructuring-bind (target pattern-constructor . pattern-args) (first pairs)
      (case pattern-constructor
        ((wildcard)
         (list (make-state :bindings bindings
                           :pairs (cdr pairs))))
        ((pattern-variables)
         (list (make-state :bindings (acons (first pattern-args)
                                            target
                                            bindings)
                           :pairs (cdr pairs))))
        ((equal-variable)
         (if (equal target (cdr (or (assoc (first pattern-args) bindings)
                                     `(nil . ,(symbol-value (first pattern-args))))))
             (list (make-state :bindings bindings
                               :pairs (cdr pairs)))
             '()))
        ((eq eql equal equalp)
         (if (funcall pattern-constructor target (eval-in (first pattern-args) bindings))
             (list (make-state :bindings bindings
                               :pairs (cdr pairs)))
             '()))
        ((=)
         (if (equal target (eval-in (first pattern-args) bindings))
             (list (make-state :bindings bindings
                               :pairs (cdr pairs)))
             '()))
        ((?)
         (if (funcall-in (first pattern-args) target bindings)
             (list (make-state :bindings bindings
                               :pairs (cdr pairs)))
             '()))
        ((^)
         (if (match-one target (first pattern-args) (lambda () t))
             '()
             (list (make-state :bindings bindings
                               :pairs (cdr pairs)))))
        ((*)
         (list (make-state :bindings bindings
                           :pairs (nconc (mapcar (lambda (p) (cons target p))
                                                 pattern-args)
                                         (cdr pairs)))))
        ((+)
         (mapcar (lambda (p)
                   (make-state :bindings bindings
                               :pairs (acons target p (cdr pairs))))
                 pattern-args))
        ((let)
         (list (make-state :bindings (nconc
                                      (mapcar (lambda (vf)
                                                (cons (car vf)
                                                      (eval-in (cdr vf) bindings)))
                                              (first pattern-args))
                                      bindings)
                           :pairs (acons target (second pattern-args)
                                         (cdr pairs)))))
        (otherwise
         (mapcar (lambda (new-pairs)
                   (make-state :bindings bindings
                               :pairs (append new-pairs (cdr pairs))))
                 (apply-pattern pattern-constructor pattern-args target)))))))

(defun state-end-p (state)
  (null (slot-value state 'pairs)))

(defun match-one (target pattern func)
  (loop
     with states = (list (make-state :pairs `((,target . ,pattern))))
     for state = (pop states)
     while state
     when (state-end-p state)
     return (values (eval-in func (state-bindings state)) t)
     do (setf states (nconc (next-states state) states))))

(defun %match (target pattern-forms)
  (loop
     for (pattern form) in pattern-forms
     do (multiple-value-bind (result success) (match-one target pattern form)
          (when success
            (return result)))
     finally (error 'failed-pattern-match)))

(defun %match-all (target pattern func)
  (loop
     with states = (list (make-state :pairs `((,target . ,pattern))))
     for state = (pop states)
     while state
     when (state-end-p state)
     collect (eval-in func (state-bindings state))
     do (setf states (nconc (next-states state) states))))


;;; cardinal macros
(defmacro match (target &rest pattern-forms)
  `(%match
    ,target
    (eval-once
     (list
      ,@(loop
           for (pattern form) in pattern-forms
           collect
             `(list (pattern-compile ',pattern)
                    (lambda-compile ',form)))))))

(defmacro match-all (target pattern form)
  `(%match-all ,target
               (eval-once (pattern-compile ',pattern))
               (eval-once (lambda-compile ',form))))

(defmacro match-lambda (&rest pattern-forms)
  (with-gensyms (target)
    `(lambda (,target)
       (match ,target ,@pattern-forms))))

(defmacro match-all-lambda (pattern form)
  (with-gensyms (target)
    `(lambda (,target)
       (match-all ,target ,pattern ,form))))
