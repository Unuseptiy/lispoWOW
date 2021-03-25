;№4
;=====================================================================================================
(defun natur (N &optional (cnt 1)) (
                  cond ((= cnt N) (cons N '()))
                  (T (cons cnt (natur N (+ cnt 1))))
                  )
    )

(print (natur 50))
;=====================================================================================================

;№9
;=====================================================================================================
(defun not-even (x &optional (parity 0)) (
                     cond ((null x) ())
                     ((= parity 1) (cons (car x) (not-even (cdr x) (- 1 parity))))
                     ((= parity 0) (not-even (cdr x) (- 1 parity)))
                     )
    )

(defun even (x &optional (parity 0)) (
                     cond ((null x) ())
                     ((= parity 1) (even (cdr x) (- 1 parity)))
                     ((= parity 0) (cons (car x) (even (cdr x) (- 1 parity))))
                     )
    )

(defun n9 (x) (list (even x) (not-even x)))

(print (n9 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
;=====================================================================================================

;№12
;=====================================================================================================
(defun dnt-rep (x) (
                       cond 
                           ((null x) ())
                           ;((not (cadr  x)) x)
                           ((eql (car x) (car (dnt-rep (cdr x)))) (dnt-rep (cdr x)))
                           (T (cons (car x) (dnt-rep (cdr x))))
                       )
    )

(print (dnt-rep '(1 1 1 2 2 2 3 3 3)))
;=====================================================================================================

;№22
;=====================================================================================================
(defun rev (x) (
                cond 
                    ((not (cadr x)) (cons (car x) '()))
                    (T (list (rev (cdr x)) (car x)))
                )
    )

(print (rev '(a b c d e f g)))
;=====================================================================================================

;№27
;=====================================================================================================
(defun split (x y &optional (parity 0)) (
              cond 
                  ((and (null x) (not (null y))) y)
                  ((and (not (null x)) (null y)) x)
                  ((= parity 0) (cons (car x) (split (cdr x) y (- 1 parity))))
                  ((= parity 1) (cons (car y) (split x (cdr y) (- 1 parity))))
              )
    )

(print (split '(1 2 3 4 5 6 7 8 9) '(a b c d e)))
;=====================================================================================================

;№30
;=====================================================================================================
(defun inf-pref (x) (
                     cond
                         ((atom x) x)
                         (T (list (nth 1 x) (inf-pref (nth 0 x)) (inf-pref (nth 2 x))))
                     )
    )

(defun вычисли (x) (eval (inf-pref x)))


(print (вычисли '((2 * 3) + (3 * 6))))
;=====================================================================================================

;№35
;=====================================================================================================
(defun subs (a x) (
                   cond
                       ((null x) nil)
                       ((eq a (car x)) T)
                       (T (subs a (cdr x)))
                   )
    )

(defun ПОДМНОЖЕСТВО (set1 set2) (
                                 cond
                                     ((and (null set1) (not (null set2))) T)
                                     ((and (subs (car set1) set2) (ПОДМНОЖЕСТВО (cdr set1) set2)))
                                     (T nil)
                                 )
    )

(defun СОБСТВЕННОЕ-ПОДМНОЖЕСТВО (set1 set2) (
                                             cond 
                                                 ((or (null set1) (and (подмножество set1 set2) (подмножество set2 set1))) nil)
                                                 ((подмножество set1 set2)  T)
                                                 (T nil)
                                             )
    )

;(print (подмножество '(2 1 3) '(1 2 3)))
(print (собственное-подмножество '(3 2) '(1 2 3)))
;=====================================================================================================
