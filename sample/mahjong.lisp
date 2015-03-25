;;;
;;;
;;; Mah-jong example
;;;
;;;

;;
;; Pattern modularization
;;
(cleson:define-pattern-function num
    (suit integer)
  (:list (= 'num) suit integer))

(cleson:define-pattern-function twin
    (pat1 pat2)
  (:multiset-cons
   (* $pat pat1)
   (:multiset-cons
    =pat
    pat2)))

(cleson:define-pattern-function shuntsu
    (pat1 pat2)
  (:multiset-cons
   (* (num $s $n) pat1)
   (:multiset-cons
    (num =s (= (+ n 1)))
    (:multiset-cons
     (num =s (= (+ n 2)))
     pat2))))

(cleson:define-pattern-function kohtsu
    (pat1 pat2)
  (:multiset-cons
   (* $pat pat1)
   (:multiset-cons
    =pat
    (:multiset-cons
     =pat
     pat2))))

;;
;; A function that determines whether the hand is completed or not.
;;
(defun complete? (hand)
  (cleson:match hand
    ((twin $th_1
           (+ (shuntsu $sh_1 (+ (shuntsu $sh_2 (+ (shuntsu $sh_3 (+ (shuntsu $sh_4 (= 'nil))
                                                                    (kohtsu $kh_1 (= 'nil))))
                                                  (kohtsu $kh_1 (kohtsu $kh_2 (= 'nil)))))
                                (kohtsu $kh_1 (kohtsu $kh_2 (kohtsu $kh_3 (= 'nil))))))
              (kohtsu $kh_1 (kohtsu $kh_2 (kohtsu $kh_3 (kohtsu $kh_4 (= 'nil))))))
           (twin $th_2 (twin $th_3 (twin $th_4 (twin $th_5 (twin $th_6 (twin $th_7 (= 'nil))))))))
     't)
    (_ 'nil)))

;;
;; Demonstration code
;;
(complete? '((Hnr (Haku)) (Hnr (Haku))
             (Num (Wan) 3) (Num (Wan) 4) (Num (Wan) 5)
             (Num (Wan) 6) (Num (Wan) 7) (Num (Wan) 8)
             (Num (Pin) 2) (Num (Pin) 3) (Num (Pin) 4)
             (Num (Sou) 6) (Num (Sou) 6) (Num (Sou) 6)))

(complete? '((Hnr (Haku)) (Hnr (Haku))
             (Num (Pin) 1) (Num (Pin) 3) (Num (Pin) 4)
             (Num (Wan) 6) (Num (Wan) 7) (Num (Wan) 8)
             (Num (Wan) 3) (Num (Wan) 4) (Num (Wan) 5)
             (Num (Sou) 6) (Num (Sou) 6) (Num (Sou) 6)))
