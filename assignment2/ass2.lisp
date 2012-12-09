(defun string-compare (str1 str2)
  (cond
    ((string= str1 str2) 'eq)
    ((string< str1 str2) 'lt)
    (t 'gt)))

(defun int-compare (i1 i2)
  (cond
    ((= i1 i2) 'eq)
    ((< i1 i2) 'lt)
    (t 'gt)))

(defun sum-int (key value sofar)
  (+ value sofar))

(defun print-keys (key value l)
  (cons key l))

(defstruct (node
  (:constructor nil)
  (:constructor new-node (key value left right)))
    key value left right)

(defstruct (dicttree
  (:constructor nil)
  (:constructor create-dictionary (&key (compare 'string-compare)))
  (:constructor new-dictionary (root &optional (compare 'string-compare))))
    root compare)

(defun lookup-aux (key node compare)
  (cond
    ((null node) '(nil nil))
    (t (case (funcall compare key (node-key node))
         (eq (list 't (node-value node)))
         (lt (lookup-aux key (node-left node) compare))
         (otherwise (lookup-aux key (node-right node) compare))))))

(defun lookup (key dict &key default)
  (let ((res (lookup-aux key (dicttree-root dict) (dicttree-compare dict))))
    (cond
      ((null (first res)) default)
      (T (second res)))))

(defun update-aux (key value root compare)
  (cond
    ((null root) (new-node key value nil nil))
    (t (case (funcall compare key (node-key root))
         (eq (new-node key
                       value
                       (node-left root)
                       (node-right root)))
         (lt
           (let ((result (update-aux key value (node-left root) compare)))
             (new-node (node-key root)
                       (node-value root)
                       result
                       (node-right root))))

         (otherwise
           (let ((result (update-aux key value (node-right root) compare)))
             (new-node (node-key root)
                       (node-value root)
                       (node-left root)
                       result)))))))


(defun update (key value dict)
  (new-dictionary (update-aux key
                              value
                              (dicttree-root dict)
                              (dicttree-compare dict))
                  (dicttree-compare dict)))

(defun fold-aux (fun root initial)
  (cond
    ((null root) initial)
    (t (fold-aux fun (node-left root)
            (funcall fun (node-key root) (node-value root)
                                  (fold-aux fun (node-right root) initial))))))

(defun fold (fun dict initial)
  (fold-aux fun (dicttree-root dict) initial))

(defun keys (dict)
  (fold 'print-keys dict ()))

(defun samekeys (dict1 dict2)
  (equal (keys dict1) (keys dict2)))

(defun balance-aux (llist)
  (let ((length (list-length llist)))
    (case length
      (0 'nil)
      (1 (new-node (first (first llist))
                   (second (first llist))
                   nil
                   nil))
      (otherwise
        (let ((pivot (floor length 2)))
          (new-node (first (nth pivot llist))
                    (second (nth pivot llist))
                    (balance-aux (subseq llist 0 pivot))
                    (balance-aux (nthcdr (+ 1 pivot) llist))))))))

(defun tree-list (key value l)
  (cons (list key value) l))

(defun balance (dict)
  (new-dictionary (balance-aux (fold 'tree-list dict ()))
                  (dicttree-compare dict)))

(defun batman (key value)
  (format t "Na~D -> ~D~%" key value))

;; Macros
(defmacro with-keys ((key value dict) body)
  (let ((root (gensym))
        (traverse (gensym)))
    `(labels ((,traverse (,root)
              (cond
                ((null ,root) 'batman)
                (t ((lambda (,key ,value) ,body)
                      (node-key ,root) (node-value ,root))
                   (,traverse (node-left ,root))
                   (,traverse (node-right ,root))))))
        (,traverse (dicttree-root ,dict)))))

#|
(defmacro match-pattern (expr &rest patterns)
  `(cond
    ((null ,patterns) nil)
    ((matching ,expr ,(first (first patterns))) (funcall ,(second (first patterns))))
    (t (,match-pattern ,(expr ,(rest patterns))))))
|#
