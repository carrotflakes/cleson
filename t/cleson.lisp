(in-package :cl-user)
(defpackage cleson-test
  (:use :cl
        :cleson
        :prove))
(in-package :cleson-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cleson)' in your Lisp.


(defmacro equals (v1 v2)
  `(match ,v1
     ((eq ,v2) 'eq)
     ((eql ,v2) 'eql)
     ((equal ,v2) 'equal)
     ((equalp ,v2) 'equalp)))


(plan nil)

(is-error (match '(1 a "b"))
          'failed-pattern-match)

(is-error (match nil)
          'failed-pattern-match)

(is (match '(1 a)
      ((:list) 0)
      ((:list _) 1)
      ((:list _ _) 2)
      ((:list _ _ _) 3))
    2)

(is (match '(1 2)
      ((= 'nil) nil)
      ((:cons (= 1) $xs) xs))
    '(2)
    :test 'equalp)

(is (equals 'a 'a)
    'eq)

(is (equals '(1) '(1))
    'equal)

(is (match-all '(1 2 3 yo "yo")
               (:multiset-cons (* (+ '2 'yo '"yo") $x) _)
               x)
    '(2 yo "yo")
    :test 'equal)

(is (match-all '(1 2)
               (:cons $x $xs)
               (list x xs))
    '((1 (2)))
    :test 'equal)

(is (match-all '(1 2 3)
               (:join $hs $ts)
               (list hs ts))
    '((NIL (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) NIL))
    :test 'equal)

(is (match-all '(1 2 3)
               (:nioj $hs $ts)
               (list hs ts))
    '((NIL (1 2 3)) ((3) (1 2)) ((2 3) (1)) ((1 2 3) NIL))
    :test 'equal)

(is (match-all '(1 2 3 4)
               (:join _ (:list $x $y))
               (list x y))
    '((3 4))
    :test 'equal)

(is (match-all '(1 2)
               (:multiset-cons $x $xs)
               (list x xs))
    '((1 (2)) (2 (1)))
    :test 'equal)

(is (match-all '(1 2)
               (:multiset-join $hs $ts)
               (list hs ts))
    '((NIL (1 2)) ((1) (2)) ((2) (1)) ((1 2) NIL))
    :test 'equal)

(is (match-all '(1 2)
               (:set-cons $x $xs)
               (list x xs))
    '((1 (1 2)) (2 (1 2)))
    :test 'equal)

(is (match-all '(1 2)
               (:set-join $hs $ts)
               (list hs ts))
    '((NIL (1 2)) ((1) (1 2)) ((2) (1 2)) ((1 2) (1 2)) ((2 1) (1 2)))
    :test 'equal)

(is (match-all '(0 1 2 3 4)
               (:multiset-cons (* (? #'evenp) $x) _) x)
    '(0 2 4)
    :test 'equal)

(is (match-all '(0 1 2 3 4)
               (:multiset-cons (* (? 'evenp) $x) _) x)
    '(0 2 4)
    :test 'equal)

(is (match-all '(0 1 2 3 4)
               (:multiset-cons (* (? (lambda (x) (evenp x))) $x) _) x)
    '(0 2 4)
    :test 'equal)

(is (match-all '(0 1 2 3 4)
               (:multiset-cons (* (+ (= 1) (= 3)) $x) _) x)
    '(1 3)
    :test 'equal)

(is (match-all '(0 1 2 3 4)
               (:multiset-cons (* (^ (= 1)) $x) _) x)
    '(0 2 3 4)
    :test 'equal)

(is (match-all '(0 1 2 3 4)
               (:multiset-cons (* (^ (^ (= 1))) $x) _) x)
    '(1)
    :test 'equal)

(is (match '(0 1)
      ((:cons $x (let ((y (+ x 1))) (:cons =y _))) (list x y)))
    '(0 1)
    :test 'equal)

(is (match-all '(1 2 3 4 1 2 1)
      (:multiset-cons
       $x
       (:multiset-cons
        (= x)
        _))
      x)
    '(1 1 2 1 1 2 1 1)
    :test 'equalp)

(is (funcall (match-lambda ((= 'yo) 'yeah)) 'yo)
    'yeah)

(is (funcall (match-all-lambda (= 'yo) 'yeah) 'yo)
    '(yeah)
    :test #'equal)

(finalize)
