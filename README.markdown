# Cleson

Cleson is [Egison](http://www.egison.org/)-like pattern-matching library for Common Lisp.
Cleson provides flexible pattern-matching against complex list structure.

Some notions are borrowed from Egison.
You should refer to http://www.egison.org/, if you want to comprehend Cleson deeply.


## Macros
### match

    (match target (pattern form) ...)


A match macro tries to match the *target* value with the each *pattern* in order. At the time of match succeed, the macro evaluates *form* and returns that value. If all of match failed, the macro signal `cleson:failed-pattern-match` error.

```common-lisp
(match
    '(hello cleson) ; target
    ((:list (= 'hello) $x) x) ; first pair of pattern and form
    (_ 'oops)) ; second pair of pattern and form
; => cleson
```

### match-all

    (match-all target pattern form)

A match-all macro tries to match the *target* value with the *pattern*. The macro returns a list which contains evaluated values of the *form* in each result of the match.

```common-liso
(match-all
    '(1 4 6 4 2 6 8 9) ; target
    (:join _ (:cons $x (:join _ (:cons (= x) _)))) ; pattern
    x) ; form
; => (4 6)
```

### match-lambda

    (match-lambda pattern-form-pair ...)

Equivalent to `(lambda (target) (match target pattern-form-pair ...))`.

### match-all-lambda

    (match-all-lambda pattern form)

Equivalent to `(lambda (target) (match-all target pattern form))`.

### define-pattern

    (define-pattern identifier lambda-list target body)

Defines new **inductive patterns** named by *identifier*.
*lambda-list* is arguments of the inductive pattern.
*target* is identifier of target.
*body* is form which to return an list of alist of target and pattern.

For example, define `:cons` pattern.
```common-lisp
(define-pattern :cons (car-pat cdr-pat) target
  (if (consp target)
      `(((,(car target) . ,car-pat) (,(cdr target) . ,cdr-pat)))
      '()))
```

### define-pattern-function

    (define-pattern-function identifier (variable ...) pattern-form)

Defines new **pattern-function** named by *identifier*.
When pattern-matching reaches a pattern like `(identifier pattern ...)`, the pattern is replaced to *pattern-form*,
and symbols which member of *variable* list in the form are replaced to the corresponding *pattern*.

For example, define `twin` pattern-function and use that.
```common-lisp
(define-pattern-function twin (pat1 pat2)
    (:multiset-cons (* pat1 $v) (:multiset-cons =v pat2)))

(match-all '(1 2 1)
    (twin $x _) x)
; => (1 1)
```


## Patterns

### Wildcard

    _

The pattern matches any target.

### Pattern variables

    $identifier

Pattern variables matches anything and the target value binds the *identifier* variable.
The variable can be used in right side of the pattern.

### Inductive patterns

    (constructor pattern ...)

Inductive patterns decompose target value in the defined method, and these decomposed values are tried match with the each *pattern*.
For example, the built-in pattern `:cons` takes 2 patterns and match a cons cell, then, tries to match the car with the first pattern and match the cdr with the second one.
You can define new inductive patterns by `define-pattern` macro.

### Value patterns

    (= form)

Value patterns match to a value which result of evaluate the *form*.
The equivalency test uses `equal` function.
You can use `eq`, `eql`, `equal` or `equalp` instead of `=`, 
these represent each equivalency test function.

#### Shorthand notation

    =identifier

Equivalent to `(= identifier)`.

### Literal patterns

    'value

If target value is considered as same as the *value* by `equal` function, the match succeeds.

### Predicate patterns

    (? function)

The *function* must be evaluated to function object.
The function is called with the target value. If that returns other than `nil`, the match succeed.

### Not-patterns

    (^ pattern)

Try to match the *pattern*. If this match does not succeed, the match succeed. otherwise consider the match failed.

### And-patterns

    (* pattern ...)

Try to match the each *pattern*. If all of these match succeeds, consider the match succeed.

### Or-patterns

    (+ pattern ...)

Try to match the first *pattern*. If that does not succeed, try the second one, etc.

### Let patterns

    (let ((identifier form) ...) pattern)

The each form is evaluated, and these result values binds corresponding *identifier* variable.
The variables can be used in the *pattern* and right side of the let pattern.


## Built-in inductive patterns
### :cons
```common-lisp
(match-all '(1 2 3) (:cons $x $xs) (list x xs))
; => ((1 (2 3)))
```

### :multiset-cons
```common-lisp
(match-all '(1 2 3) (:multiset-cons $x $xs) (list x xs))
; => ((1 (2 3)) (2 (1 3)) (3 (1 2)))
```

### :set-cons
```common-lisp
(match-all '(1 2 3) (:set-cons $x $xs) (list x xs))
; => ((1 (1 2 3)) (2 (1 2 3)) (3 (1 2 3)))
```

### :join
```common-lisp
(match-all '(1 2 3) (:join $hs $ts) (list hs ts))
; => ((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ()))
```

### :list
```common-lisp
(match '(a)
    ((:list) 'zero)
    ((:list _) 'one)
    ((:list _ _) 'two))
; => one
```

### others

Refer to `lib/core/*`.

## Example

### Poker hands
```common-lisp
(define-pattern-function card (suit number)
  (:list (= 'card) suit number))

(define-pattern-function hand (a b c d e)
  (:multiset-cons
   a
   (:multiset-cons
    b
    (:multiset-cons
     c
     (:multiset-cons
      d
      (:multiset-cons
       e
       (= 'nil)))))))

(defun poker-hands (cs)
  (match cs
    ((hand (card $s $n)
           (card =s (= (1+ (mod (- n 2) 13))))
           (card =s (= (1+ (mod (- n 3) 13))))
           (card =s (= (1+ (mod (- n 4) 13))))
           (card =s (= (1+ (mod (- n 5) 13)))))
     '(straight-flush))
    ((hand (card _ $n)
           (card _ =n)
           (card _ =n)
           (card _ =n)
           card)
     '(four-of-kind))
    ((hand (card _ $m)
           (card _ =m)
           (card _ =m)
           (card _ $n)
           (card _ =n))
     '(full-house))
    ((hand (card $s _)
           (card =s _)
           (card =s _)
           (card =s _)
           (card =s _))
     '(Flush))
    ((hand (card _ $n)
           (card _ (= (1+ (mod (- n 2) 13))))
           (card _ (= (1+ (mod (- n 3) 13))))
           (card _ (= (1+ (mod (- n 4) 13))))
           (card _ (= (1+ (mod (- n 5) 13)))))
     '(straight))
    ((hand (card _ $n)
           (card _ =n)
           (card _ =n)
           _
           _)
     '(three-of-kind))
    ((hand (card _ $m)
           (card _ =m)
           (card _ $n)
           (card _ =n)
           _)
     '(two-pair))
    ((hand (card _ $m)
           (card _ =m)
           _
           _
           _)
     '(one-pair))
    (_
     '(nothing))))

(poker-hands '((Card (Club) 12)
               (Card (Club) 10)
               (Card (Club) 13)
               (Card (Club) 1)
               (Card (Club) 11)))
; => (straight-flush)
```

## Installation

1. Install [quicklisp](http://www.quicklisp.org/beta/).
2. Get Cleson repository, and place in `quicklisp/local-projects/` directory.
3. `(ql:quicklisp :cleson)`

## Author

* carrotflakes (<carrotflakes@gmail.com>)

## Copyright

Copyright (c) 2015 carrotflakes

## License

Licensed under the LLGPL License.
