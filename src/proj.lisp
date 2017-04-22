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
			  (next-init (setf init-list (append init-list (list init-arg arg))))
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
		,(create-recognizer class-string class-name class-list)

		; create getter of all slot names
		(defun ,(intern (concatenate 'string class-string "-SLOTS")) ()
			',parsed-slots)

		; create getter of superclasses
		(defun ,(intern (concatenate 'string class-string "-SUPERCLASSES")) ()
			',(rest class-list))

		; create a getter for default initializations
		(defun ,(intern (concatenate 'string class-string "-DEFAULTS")) ()
			',init-list))))	

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
;; class-list: a list of the class's superclasses and class itself
(defun create-recognizer (class-string class-name class-list)
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