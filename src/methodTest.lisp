(load "proj.lisp")
(def-class person age)
(def-class (man person))
(def-generic sum (p1 p2))
(def-method sum ((person p1) (person p2)) (format t "PP")(+ (person-age p1) (person-age p2)))
(def-method sum ((person p1) (man p2)) (format t "PM") (+ (person-age p1) (man-age p2)))
(sum (make-person :age 1) (make-man :age 2))