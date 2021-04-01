;№44
;Определите функцию, проверяющую является ли одно дерево поддеревом другого.
;=====================================================================================================
(defun isnode (node tree) (
                           cond
                               ((null tree) nil)
                               (t (or (eq node (car tree)) (isnode node (cadr tree)) (isnode node (caddr tree))))
                           )
    )

(defun isparent (P C tree) (
                              cond
                                ((null tree) nil)
                                ((and (eq (car tree) P) (or (eq (caadr tree) C) (eq (caaddr tree) C))) t)
                                (t (or (isparent P C (cadr tree)) (isparent P C (caddr tree))))
                              )
)

(defun subtree (tree1 tree2) (
                              cond
                                  ((atom tree1) (isnode tree1 tree2))
                                  (T (and (subtree (cadr tree1) tree2) (subtree (caddr tree1) tree2) (isparent (car tree1) (caadr tree1) tree2) (isparent (car tree1) (caaddr tree1) tree2)))
                              )
    )

(print (subtree '(1 nil nil) '(1 nil nil)))
