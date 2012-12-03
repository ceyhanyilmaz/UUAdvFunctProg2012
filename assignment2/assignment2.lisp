; Structs
(defstruct treedict root compare)
(defstruct treenode key value leftchild rightchild)

; Functions
(defun stringcompare (str1 str2)
	; TODO: Fixa för case
	(if (string= str1 str2) 
		EQ
	  (if (string< str1 str2) 
		  LT
		GT)))

(defun create-dictionary (&key compare)
	(if compare
		(make-treedict :root nil :compare compare)
	  (make-treedict :root nil :compare #'stringcompare)))

(defun updatenode (key value node 'compare)
	(if (null node)
	  	(make-treenode :key key :value value :leftchild nil :rightchild nil)
	  (case (compare key node-key)
	  	(LT (make-treenode :key node-key :value node-value
	  	 :leftchild (updatenode key value leftchild 'compare) 
	  	 :rightchild node-rightchild))
	  	(GT (make-treenode :key node-key :value node-value
	  	 :leftchild node-lefchild
	  	 :rightchild (updatenode key value rightchild 'compare)))
	  	(EQ (make-treenode :key key :value value
	  		:leftchild leftchild :rightchild rightchild)))))

(defun update (key value dict)
	(make-treedict :root (updatenode key value 'dict-root 'dict-compare)
		:compare 'dict-compare))

(defun lookup (key dict &key default)) 

(defun fold (dict initial)) 

(defun rebalance (dict)) 

(defun keys (dict)) 

(defun samekeys (dict1 dict2))  