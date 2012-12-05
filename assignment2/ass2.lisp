;; How handle dict = nil??!?

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
         (eq (new-node key value (node-left root) (node-right root)))
         (lt (new-node (node-key root) (node-value root)
                       (update-aux key value (node-left root) compare)
                       (node-right root)))
         (otherwise (new-node (node-key root) (node-value root)
                              (node-left root)
                              (update-aux key value
                                          (node-right root) compare)))))))

(defun update (key value dict)
  (new-dictionary (update-aux key value (dicttree-root dict)
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


#|
(defun balance-aux (root)
  (cond
    ((null root) '(nil 0 0))
    (t (let ((left balance-aux(node-left root))
             (right balance-aux(node-right root))
             (left-bal (- (second left) (third left)))
             (right-bal (- (second right) (third right)))
             (bal (- left-bal right-bal)))
         (cond
           ((bal < -1) LEFTRIGHT/LEFTLEFT)
           ((bal > 1) RIGHTLEFT/RIGHTRIGHT)
           (t (list (new-node (node-key root) (node-valueroot)
                              (first left) (first right))
                     (+ 1 (max (second left) (third left)))  
                      (+ 1 (max (second right) (third right))))))))))
|#
