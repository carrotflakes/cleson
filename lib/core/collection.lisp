(in-package :cleson)


;;; cons-list
(define-pattern :cons (car cdr) target
  (if (consp target)
      `(((,(car target) . ,car) (,(cdr target) . ,cdr)))
      '()))

(define-pattern :join (left right) target
  (let ((ts '()))
    (nconc
     (loop
        for hs on target
        collect `((,ts . ,left) (,hs . ,right))
        do (setf ts (append ts (list (car hs)))))
     `(((,ts . ,left) (() . ,right))))))

(define-pattern :nioj (left right) target
  (let ((ts '()))
    (reverse
     (nconc
      (loop
         for hs on target
         collect `((,hs . ,left) (,ts . ,right))
         do (setf ts (append ts (list (car hs)))))
      `(((,() . ,left) (,ts . ,right)))))))

(define-pattern :list (&rest args) target
  (if (= (length args) (length target))
      `(,(mapcar #'cons target args))
      '()))


;;; multiset
(define-pattern :multiset-cons (car cdr) target
  (let ((ts '()))
    (loop
       for hs on target
       collect `((,(car hs) . ,car) (,(append (cdr hs) ts) . ,cdr))
       do (setf ts (append ts (list (car hs)))))))

(define-pattern :multiset-join (left right) target
  (labels ((rec (i rest rest-length set-1 set-2)
             (if rest
                 (nconc (when (< i rest-length)
                          (rec i (cdr rest) (1- rest-length)
                               set-1 (cons (car rest) set-2)))
                        (unless (zerop i)
                          (rec (1- i) (cdr rest) (1- rest-length)
                               (cons (car rest) set-1) set-2)))
                 `(((,set-1 . ,left) (,set-2 . ,right))))))
    (loop
       with length = (length target)
       for i to length
       nconc (rec i (reverse target) length '() '()))))


;;; set
(define-pattern :set-cons (car cdr) target
  (loop
     for c in target
     collect `((,c . ,car) (,target . ,cdr))))

(define-pattern :set-join (left right) target
  (labels ((rec (i rest set)
             (if (zerop i)
                 `(((,(reverse set) . ,left) (,target . ,right)))
                 (let ((ts '()))
                   (loop
                      for hs on rest
                      nconc (rec (1- i) (append (cdr hs) ts) (cons (car hs) set))
                      do (setf ts (append ts (list (car hs)))))))))
    (loop
       with length = (length target)
       for i to length
       nconc (rec i target '()))))
