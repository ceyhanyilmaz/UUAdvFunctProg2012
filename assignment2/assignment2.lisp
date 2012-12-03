
; Structs
(defstruct treenode key value leftchild rightchild)

; Functions
;(make-treedict :nodes (make-treenode) :compare compare)

(defun make-treedict (parameter*)

	) 

(defun create-dictionary (&key compare)

	) 

(defun lookup (key dict &key default)) 

(defun update (key value dict)) 

(defun fold (dict initial)) 

(defun rebalance (dict)) 

(defun keys (dict)) 

(defun samekeys (dict1 dict2))  