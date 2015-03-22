;;;
;;;
;;; Poker-hands demonstration
;;;
;;;

;;
;; Pattern definitions
;;
(cleson:define-pattern-function card (suit number)
  (:list (= 'card) suit number))

(cleson:define-pattern-function hand (a b c d e)
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

;;
;; A function that determines poker-hands
;;
(defun poker-hands (cs)
  (cleson:match cs
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

;;
;; Demonstration code
;;
(defun test (cards)
  (print (poker-hands cards)))

(test '((Card (Club) 12)
        (Card (Club) 10)
        (Card (Club) 13)
        (Card (Club) 1)
        (Card (Club) 11)))

(test '((Card (Diamond) 1)
        (Card (Club) 2)
        (Card (Club) 1)
        (Card (Heart) 1)
        (Card (Diamond) 2)))

(test '((Card (Diamond) 4)
        (Card (Club) 2)
        (Card (Club) 5)
        (Card (Heart) 1)
        (Card (Diamond) 3)))

(test '((Card (Diamond) 4)
        (Card (Club) 10)
        (Card (Club) 5)
        (Card (Heart) 1)
        (Card (Diamond) 3)))

