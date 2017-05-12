(defparameter *methods-def* (make-hash-table :test 'equal))
(defparameter *methods-def-before* (make-hash-table :test 'equal))
(defparameter *methods-def-after* (make-hash-table :test 'equal))
(defparameter *methods-id* (make-hash-table :test 'equal))

;; Description: Implements the "def-class" macro
;;
;; classes: either the class name or a list of the class and superclasses
;; slots: the class slots; may have default initializations (e.g., "age :initform 22")
(defmacro def-class (classes &rest slots)
	(let ((class-string nil)
		  (class-name nil)
		  (class-list nil)
		  (slot-list '())
		  (next-init nil)
		  (init-list '())
		  (init-arg nil)
		  (parsed-slots '()))

	; builds the initializers list
	(dolist (arg slots) 
		(cond ((eq arg ':initform) (setf next-init t))
			  (next-init (setf init-list (append init-list (list init-arg arg))) (setf next-init nil))
			  (t (setf init-arg arg) (setf next-init nil) (setf parsed-slots (append parsed-slots (list arg))))))

	; parses the declared (super)classes
	(if (typep classes 'list) 
			(progn (setf class-name (first classes))
				   (setf class-list (nconc (list class-name) (build-superclass-list classes)))
				   (setf slot-list parsed-slots)
				   (dolist (class (rest class-list))
				   		(setf slot-list (append slot-list (funcall (symbol-function (intern (concatenate 'string (string-upcase (symbol-name class)) "-SLOTS")))))))
				   (setf slot-list (remove-duplicates slot-list)))
				   
			(progn (setf class-name classes)
					(setf class-list (list classes))
					(setf slot-list parsed-slots)))

	; add the superclasses' initializers
	(setf init-list (append init-list (build-superclass-defaults (rest class-list))))
	(setf class-string (string-upcase (symbol-name class-name)))

	`(progn 
		; create constructor
		,(create-constructor class-string class-list slot-list init-list)
		; create getters
		,@(map 'list (lambda (slot) (create-getter class-name slot)) slot-list)
		; create setters
		,@(map 'list (lambda (slot) (create-setter class-name slot)) slot-list)		
		; create recognizer
		,(create-recognizer class-string class-name)
		; create getter of all slot names
		(defun ,(intern (concatenate 'string class-string "-SLOTS")) ()
			',parsed-slots)
		; create getter of superclasses
		(defun ,(intern (concatenate 'string class-string "-SUPERCLASSES")) ()
			',(rest class-list))
		; create a getter for default initializations
		(defun ,(intern (concatenate 'string class-string "-DEFAULTS")) ()
			',init-list)
		t)))	

(defmacro def-generic (method-name (&rest arguments))		
	`(defun ,method-name ,arguments
		(let ((classes nil)
			(stored-methods nil)
			(best-meth nil)
			(other-meth nil)
			(arg-index 0)
			(arg-classes nil))

		,@(map 'list (lambda (arg) `(setf classes (append classes (list (gethash 'classes ,arg))))) arguments)

		;(format t "A: ~A~%" ',arguments)
		;(format t "Arg classes: ~A~%" classes)

		(setf stored-methods (gethash ',method-name *methods-def*))
		;(format t "stored-methods: ~A~%" stored-methods)

		; compare methods
		(setf found-class nil)
		(setf best-meth (nth 0 stored-methods))
		(dolist (other-meth stored-methods)
			;(format t "Trying method ~A~%" other-meth)
			(setf arg-index 0)

			; compare methods argument by argument
			(block outer (loop 
				(when (eq arg-index (length classes)) (return-from outer))
				(setf best-meth-arg (nth arg-index (second best-meth)))
				(setf other-meth-arg (nth arg-index (second other-meth)))
				(setf arg-classes (nth arg-index classes))

				;(format t "Best Meth: ~A~%" best-meth)
				;(format t "Other Meth: ~A~%" other-meth)

				(block inner 
					(cond 
						((and (member best-meth-arg arg-classes) (equal best-meth-arg other-meth-arg)) 
											(setf found-class t) 
											;(format t "Same arg ~A ~%" best-meth-arg)
											(return-from inner)) 
						(t
							; search for a class that matches the argument
						  	(dolist (class arg-classes)
						  		;(format t "best-meth-arg: ~A~%" best-meth-arg)
						  		;(format t "other-meth-arg: ~A~%" other-meth-arg)
						  		;(format t "class: ~A~%" class)

						  		(cond ((equal best-meth-arg class) (setf found-class t) 
						  											;(format t "Found ~A~%" best-meth-arg) 
						  											(return-from inner))
						  			  ((equal other-meth-arg class) (setf found-class t) 
						  			  								(setf best-meth other-meth) 
						  			  								;(format t "Found ~A~%" best-meth-arg)
;						  			  								(format t "Setting best to ~A~%" other-meth) 
						  			  								(return-from inner)))
						  		(setf found-class nil))))
					; we know at least one method applies
					)
				(setf arg-index (1+ arg-index)))))

		(if (not found-class) (error "Method '~A' can't be applied to arguments of classes ~A~%" ',method-name classes))
		(funcall (intern (first best-meth)) ,@arguments))))


;; Description: Allows for the definition of a method along with parameter specifiers 
;; e.g., (def-method sum ((person p1) (person p2)) 
;;				(sum (person-age p1) (person-age p2)))
;;
;; method-name: the method's name
;; arguments: the list where each element may be an argument or a list with 
;;	 both the class and the argument
;; body: the method body
(defmacro def-method (method-name (&rest arguments) &rest body)
	(let ((arg-classes nil)
		  (arg-instance nil)
		  (class nil)
		  (method-id nil)
		  (new-method-name nil))
		
		(cond 	((eq (first arguments) ':before) 
			  		(error "Qualifier :before not implemented"))
			  	((eq (first arguments) ':after) 
			  		(error "Qualifier :after not implemented"))
				(t	
					(dolist (arg arguments)
						(if (listp arg) 
								(progn (setf arg-classes (append arg-classes (list (first arg))))
									(setf arg-instance (append arg-instance (list (second arg)))))
								(progn (setf arg-classes (append arg-classes '(t)))
									(setf arg-instance (append arg-instance (list arg)))))
					)
					
					; obtain method id
					(if (eq (gethash method-name *methods-id*) nil) 
							(setf method-id 1)
							(setf method-id (1+ (gethash method-name *methods-id*))))
					
					(setf (gethash method-name *methods-id*) method-id)
					(setf new-method-name (concatenate 'string (string-upcase (symbol-name method-name)) "-" (write-to-string method-id)))
					(setf (gethash method-name *methods-def*) (append (gethash method-name *methods-def*) (list (list new-method-name arg-classes)))))

			  )

		`(defun ,(intern new-method-name) (,@arg-instance) ,@body)))

;; Description: Creates a constructor for class "class-string"
;;
;; class-string: the name of the class being defined
;; class-list: a list of the class's superclasses and class itself
;; slot-list: the slots the class must hold (both the superclasses' slots and its own)
;; init-list: list with the default initializations
(defun create-constructor (class-string class-list slot-list init-list)
	`(defun ,(intern (concatenate 'string "MAKE-" class-string)) (&optional &key ,@slot-list)
		(let ((object (make-hash-table :test 'equal)))
			(setf (gethash 'classes object) ',class-list)
			(dotimes (i (length ',init-list)) 
				(if (eq (mod i 2) 0)
					(setf (gethash (nth i ',init-list) object) (eval (nth (1+ i) ',init-list)))))			
			,@(map 'list (lambda (slot) `(if ,slot (setf (gethash ',slot object) ,slot))) slot-list)
			object)))

;; Description: Creates a recognizer for class "class-name"
;;
;; class-string: the name of the class being defined
;; class-name: the symbol of the class being defined
(defun create-recognizer (class-string class-name)
	`(defun ,(intern (concatenate 'string class-string "?")) (,class-name) 
		(let ((class-list (multiple-value-bind (value _) 
						(gethash 'classes ,class-name)
						(declare (ignore _))
						value))) 
			(numberp (position ',class-name class-list :test #'equal)))))

;; Description: Creates a getter for the slot "slot"
;;
;; class-name: the name of the class being defined
;; slot: the name of the slot for which the getter is to be created
(defun create-getter (class-name slot)
	`(defun ,(intern (concatenate 'string (string-upcase (symbol-name class-name)) 
											"-" (symbol-name slot))) (,class-name)
		(multiple-value-bind (value _) 
			(gethash ',slot ,class-name)
			(declare (ignore _))
				value)))

;; Description: Creates a setter for the slot "slot"
;;
;; class-name: the name of the class being defined
;; slot: the name of the slot for which the getter is to be created
(defun create-setter (class-name slot)
	`(defun ,(intern (concatenate 'string "SET-" (string-upcase (symbol-name class-name)) 
											"-" (symbol-name slot))) (,class-name new-value)
		(setf (gethash ',slot ,class-name) new-value)))

;; Description: Builds the entire superclass list based on the declared superclasses
;;
;; class-list: classes declared in the class definition
(defun build-superclass-list (class-list)
	(let ((all-classes (rest class-list))
		  (previous-list nil)
		  (superclasses nil))
		
		(do ()
			((equal previous-list all-classes) all-classes)
			(setf previous-list all-classes)

			(dolist (class previous-list)
				(setf superclasses (funcall (symbol-function (intern (concatenate 'string 
																	(string-upcase (symbol-name class))
																	"-SUPERCLASSES")))))
				(setf all-classes (append all-classes superclasses)))
			(setf all-classes (remove-duplicates all-classes)))))

;; Description: Returns a list of default initializers for the class list
;;
;; class-list: list of superclasses
(defun build-superclass-defaults (class-list)
	(let ((defaults '()))
		(dolist (class class-list)
			(setf defaults (append defaults (funcall (symbol-function (intern (concatenate 'string 
																	(string-upcase (symbol-name class)) 
																	"-DEFAULTS")))))))
		defaults))