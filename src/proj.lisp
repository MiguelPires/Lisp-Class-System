(defmacro def-class (classes &rest slots)
	(let ((class-string nil)
		  (class-name nil)
		  (class-list nil)
		  (slot-list '()))

	(if (typep classes 'list) 
			(progn (setf class-name (first classes))
				   (setf class-list (nconc (list class-name) (build-superclass-list classes)))
				   (setf slot-list slots)
				   (dolist (class (rest class-list))
				   		(setf slot-list (append slot-list (funcall (symbol-function (intern (concatenate 'string (string-upcase (symbol-name class)) "-SLOTS")))))))
				   (setf slot-list (remove-duplicates slot-list)))
				   
			(progn (setf class-name classes)
					(setf class-list (list classes))
					(setf slot-list slots)))

	(setf class-string (string-upcase (symbol-name class-name)))

	`(progn 
		; create constructor
		,(create-constructor class-string class-list slot-list)

		; create getters
		,@(map 'list (lambda (slot) (create-getter class-name slot)) slot-list)

		; create setters
		,@(map 'list (lambda (slot) (create-setter class-name slot)) slot-list)		

		; create recognizer
		,(create-recognizer class-string class-name class-list)

		; create getter of all slot names
		(defun ,(intern (concatenate 'string class-string "-SLOTS")) ()
			',slots)

		; create getter of superclasses
		(defun ,(intern (concatenate 'string class-string "-SUPERCLASSES")) ()
			',(rest class-list)))))	


;; Description: Creates a constructor for class "class-string"
;;
;; class-string: the name of the class being defined
;; class-list: a list of the class's superclasses and class itself
;; slot-list: the slots the class must hold (both the superclasses' slots and its own)
(defun create-constructor (class-string class-list slot-list)
	`(defun ,(intern (concatenate 'string "MAKE-" class-string)) (&key ,@slot-list)
		(let ((object (make-hash-table :test 'equal)))
			(setf (gethash 'classes object) ',class-list)
			,@(map 'list (lambda (slot) (create-slot slot)) slot-list)
			object)))

;; Description: Creates a recognizer for class "class-name"
;;
;; class-string: the name of the class being defined
;; class-name: the symbol of the class being defined
;; class-list: a list of the class's superclasses and class itself
(defun create-recognizer (class-string class-name class-list)
	`(defun ,(intern (concatenate 'string class-string "?")) (,class-name) 
		(let ((class-list (multiple-value-bind (value _) 
						(gethash 'classes ,class-name)
						(declare (ignore _))
						value))) 
			(numberp (position ',class-name class-list :test #'equal))))
)
;; Description: Creates a getter for the slot "slot"
;;
;; class-name: the name of the class being defined
;; slot: the name of the slot for which the getter is to be created
(defun create-getter (class-name slot)
	`(defun ,(intern (concatenate 'string (string-upcase (symbol-name class-name)) 
											"-" (symbol-name slot))) (,class-name)
		(multiple-value-bind (value bool) 
			(gethash ',slot ,class-name)
				value)))

;; Description: Creates a setter for the slot "slot"
;;
;; class-name: the name of the class being defined
;; slot: the name of the slot for which the getter is to be created
(defun create-setter (class-name slot)
	`(defun ,(intern (concatenate 'string "SET-" (string-upcase (symbol-name class-name)) 
											"-" (symbol-name slot))) (,class-name new-value)
		(setf (gethash ',slot ,class-name) new-value)))


(defun create-slot (symbol)
	`(setf (gethash ',symbol object) ,symbol))

;; Description: Builds the entire superclass list based on the declared superclasses
;;
;; class-list: classes declared in the class definition
(defun build-superclass-list (class-list)
	(let ((all-classes (rest class-list))
		(previous-list nil))
		
		(do ()
			((equal previous-list all-classes) all-classes)

			(setf previous-list all-classes)
			(dolist (class all-classes)
				(setf superclasses (funcall (symbol-function (intern (concatenate 'string 
																	(string-upcase (symbol-name class))
																	"-SUPERCLASSES")))))
				(nconc all-classes superclasses))
			(setf all-classes (remove-duplicates all-classes)))))