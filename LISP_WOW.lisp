;№4
;Определите функцию, порождающую по заданному натуралльному числу N список, состоящий из натуральных чисел от 1 до N 
;=====================================================================================================
(defun natur (N &optional (cnt 1)) (
                  cond ((= cnt N) (cons N '()))
                  (T (cons cnt (natur N (+ cnt 1))))
                  )
    )

(print '№4)
(print (natur 10))
(print (natur 1))
(print (natur 0))
;=====================================================================================================

;№9
;Определите функцию, разделяющую исходный список на два подсписка. В первый из них должны попасть элементы с нечетными номерами, во второй -- элементы с четными номерами.
;=====================================================================================================
(defun not-even (x &optional (parity 0)) (
                                          (lambda (tail-x ch-parity) (
                                                        cond 
                                                            ((null x) ())
                                                            ((= parity 1) (cons (car x) (not-even tail-x ch-parity)))
                                                            ((= parity 0) (not-even tail-x ch-parity))
                                                        )
                                                  ) (cdr x) (- 1 parity)
                                          )
    )

(defun even (x &optional (parity 0)) (
                                      (lambda (tail-x ch-parity) (
                                                                  cond 
                                                                      ((null x) ())
                                                                      ((= parity 1) (even tail-x ch-parity))
                                                                      ((= parity 0) (cons (car x) (even tail-x ch-parity)))
                                                                  )
                                              ) (cdr x) (- 1 parity)
                     )
    )

(defun n9 (x) (list (even x) (not-even x)))

(print '№9)
(print (n9 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
(print (n9 '(1 2 3 4 5)))
(print (n9 '(1 2)))
;=====================================================================================================

;№12
;Определите функцию, заменяющую в исходном списке два подряд идущих одинаковых элемента одним
;=====================================================================================================
(defun dnt-rep (x) (
                    (lambda (head-x tail-x) (
                                             cond
                                                 ((null x) ())
                                                 ;((not (cadr  x)) x)
                                                 ((eql head-x (car (dnt-rep tail-x))) (dnt-rep tail-x))
                                                 (T (cons head-x (dnt-rep tail-x)))
                                             )
                            ) (car x) (cdr x)
                       )
    )

(print '№12)
(print (dnt-rep '(1 1 1 2 2 2 3 3 3)))
(print (dnt-rep '(1 1 2 3 4 5)))
(print (dnt-rep '(1 1 1 1 1 1 1 1 1)))
;=====================================================================================================

;№22
;Определите функцию, которая обращает список (a b c) и разбивает его на уровни (((c) b) a).
;=====================================================================================================
(defun rev (x) (
                (lambda (head-x) (
                                  cond 
                                      ((not (cadr x)) (cons (car x) '()))
                                      (T (list (rev (cdr x)) (car x)))
                                  )
                        ) (car x)
                )
)

(print '№12)
(print (rev '(a b c)))
(print (rev '(a b c d e f g)))
(print (rev '(1 2 3 4 5 6 7)))
;=====================================================================================================

;№27
;Определите функцию, которая, чередуя элементы списков (a b ...) и (1 2 ...), образует новый список (а 1 b 2 ...).
;=====================================================================================================
(defun split (x y &optional (parity 0)) (
                                         (lambda (nll-x nll-y ch-parity) (
                                                                            cond
                                                                                ((and nll-x (not nll-y)) y)
                                                                                ((and (not nll-x) nll-y) x)
                                                                                ((= parity 0) (cons (car x) (split (cdr x) y ch-parity)))
                                                                                ((= parity 1) (cons (car y) (split x (cdr y) ch-parity)))
                                                                            )
                                            ) (null x) (null y) (- 1 parity)
                                        )
)

(print '№27)
(print (split '(1 2 3 4 5 6 7 8 9) '(a b c d e)))
(print (split '(1 2 3) '(a b c)))
(print (split '(1 2 3) '(a b c d e)))
;=====================================================================================================

;№30
;Запрограммируйте интерпретатор ВЫЧИСЛИ, который преобразует инфиксную запись операций в префиксную и возвращает значение выражения.
;=====================================================================================================
(defun inf-pref (x) (
                     cond
                         ((atom x) x)
                         (T (list (cadr x) (inf-pref (car x)) (inf-pref (caddr x))))
                     )
    )

(defun вычисли (x) (eval (inf-pref x)))


(print '№30)
(print (вычисли '((2 * 3) + (3 * 6))))
(print (вычисли '((-2 + 4) * 3)))
(print (вычисли '((2 + 3) * (3 + 6))))
;=====================================================================================================

;№35
;Определите функцию ПОДМНОЖЕСТВО, которая проверяет, является ли одно множество подмножеством другого. Определите также СОБСТВЕННОЕ-ПОДМНОЖЕСТВО.
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

(print '№35)
(print (подмножество '(2 1 3) '(1 2 3)))
(print (подмножество '(1 2 3 4) '(1 2 3)))
(print (подмножество '(2 1 3) '(1 2 3 4 5)))

(print (собственное-подмножество '(3 2) '(1 2 3)))
(print (собственное-подмножество '(1 2 3) '(1 2 3)))
(print (собственное-подмножество '() '(1 2 3)))
;=====================================================================================================

;№42
;Опрделите функцию, находящую максимальное из значений, находящихся в вершинах дерева.
;=====================================================================================================
(defun max2 (x y) (
                   cond
                       ((> x y) x)
                       (t y)
                   )
    )

(defun max3 (x y z) (max2 x (max2 y z)))

(defun tree-max (x) (
                     cond
                         ((null x) -100)
                         (t (max3 (cadddr x) (tree-max (caddr x)) (tree-max (cadr x))))
                     )
    )

(print '№42)
(print (tree-max '(1 nil nil 1)))
(print (tree-max '(1 
                    (2
                      (4 
                        nil 
                        nil 
                        4
                      )
                      (5 
                        nil 
                        (7 nil nil 7)
                        5
                      )
                      2
                    )
                    (3
                      nil
                      (6 nil nil 6)
                      3
                    )
                    1
                  )
       )
)

(print (tree-max '(1 
                    (2
                      (4 
                        nil 
                        nil 
                        4
                      )
                      (5 
                        nil 
                        (7 nil nil 7)
                        5
                      )
                      2
                    )
                    (3
                      nil
                      (6 nil nil 6)
                      3
                    )
                    10
                  )
       )
)
;=====================================================================================================

;№44
;Определите функцию, проверяющую является ли одно дерево поддеревом другого.
;=====================================================================================================
(defun isleaf (leaf tree) (
                           (lambda (head L-subtree R-subtree) (
                                              cond 
                                                  ((and (null L-subtree) (null R-subtree)) (eq leaf head)) ;zdes ponyat leaf - its list or not (sdelano dlya ne lista)
                                                  (T (or (isleaf leaf L-subtree) (isleaf leaf R-subtree)))
                                              )
                                   ) (car tree) (cadr tree) (caddr tree)
                           )
    )


(defun isparent (P L R tree) (
                              (lambda (head L-subtree R-subtree L-subtree-head R-subtree-head) (
                                                                  cond
                                                                      ((or (null L-subtree-head) (null R-subtree-head)) nil)
                                                                      ((and (= P head) (or (and (= L L-subtree-head) (= R R-subtree-head)) (and (= L R-subtree-head) (= R L-subtree-head)))))
                                                                      (T (or (isparent P L R L-subtree) (isparent P L R R-subtree)))
                                                                  )
                                      ) (car tree) (cadr tree) (caddr tree) (caadr tree) (caaddr tree)
    )
)

(defun isnode (node tree) (
                           cond
                               ((null tree) nil)
                               (t (or (= node (car tree)) (isnode node (cadr tree)) (isnode node (caddr tree))))
                           )
    )

(defun subtree (tree1 tree2) (
                              cond
                                  ((atom tree1) (isnode tree1 tree2))
                                  (T (and (subtree (cadr tree1) tree2) (and (subtree (caddr tree1) tree2) (isparent (car tree1) (caadr tree1) (caaddr tree1) tree2))))
                              )
    )

(print (subtree '(1
                      (2 nil nil)
                      (3 nil nil)
                  ) 
                '(1
                      (2 nil nil)
                      (3 nil nil)
                  )
         )
)

;=====================================================================================================

;№45
;Предположим, что у имени города есть свойства x и y, которые содержат координаты места нахождения города относительно
;некоторого начала координат. Напишите функцию (РАССТОЯНИЕ a b), вычисляющую расстояние между городами a и b.
(defun РАССТОЯНИЕ (a b) (
                         sqrt (+ (* (- (get b 'x) (get a 'x)) (- (get b 'x) (get a 'x))) (* (- (get b 'y) (get a 'y)) (- (get b 'y) (get a 'y))))
                         )
    )
