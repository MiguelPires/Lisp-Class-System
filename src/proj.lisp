(defmacro def-class (class-name &rest slots)
	(let ((class-string (string-upcase (symbol-name class-name)))
		(slot-index 0))

	`(progn 
		(defun ,(intern (concatenate 'string "MAKE-" class-string)) (&key ,@slots)
			(vector (list ,class-string) ,@slots))
		,@(map 'list (lambda (slot) (create-getter class-name slot (position slot slots))) slots)
		(defun ,(intern (concatenate 'string class-string "?")) (,class-name) 
			(numberp (position (string-upcase (symbol-name ',class-name)) (aref ,class-name 0) :test #'equal)))
	))
)

;; Description: Creates a getter for the slot "slot" 
;;
;; class-name: the name of the class being defined
;; slot: the name of the slot for which the getter is to be created
;; slot-index: the slot's index in the internal representation
(defun create-getter (class-name slot slot-index)
	`(defun ,(intern (concatenate 'string (string-upcase (symbol-name class-name)) 
											"-" 
											(symbol-name slot))) (,class-name)
				(aref ,class-name (+ ,slot-index 1))))

