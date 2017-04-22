;(compile-file "proj.lisp")

(load "proj.lisp")

	(def-class person
		name
		age)

	(def-class (student person)
		course)

	(format t "~A~%" (student-slots))

	(format t "~A~%" (let ((s (make-student :name "Paul" :age 21 :course "Informatics")))
		(list (student-name s) (student-course s))))

	(format t "~A~%" (let ((s (make-student :name "Paul" :age 21 :course "Informatics")))
		(list (person-name s) (student-course s))))

	(format t "~A~%" (let ((p (make-person :name "John" :age 34))
		(s (make-student :name "Paul" :age 21 :course "Informatics")))
		(list (person? p) (student? p) (person? s) (student? s))))

	(def-class sportsman
		activity
		schedule)
	
	(def-class (ist-student student sportsman))

	(format t "~A~%" (let ((m (make-ist-student :name "Maria" :course "IA" :activity "Tennis")))
		(list (ist-student? m)
		(student? m)
		(sportsman? m)
		(ist-student-name m)
		(person-name m)
		(sportsman-activity m)
		(ist-student-activity m))))