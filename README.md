# Lisp Class System
This project implements a class system for Lisp through a set of macros. The implemented features are:

* Classes 
* Default Slot Initializers
* Multiple Inheritance
* Methods
* Generics

## Classes
The *def-class* macro allows you to create a class complete with default constructor, getters, setters, recognizers. For instance, to create a 
class *person* with instance variables (or slots) *name* and *age*:

`(def-class person name age)`

To use the default methods do:
```
>(setf p (make-person :age 50 :name John))
#S(...) 
>(person-age p)
50
>(set-person-name p "Bill")
"Bill"
>(person-name p)
"Bill"
>(person? p)
t
```

## Default Slot Initializers
It's possible to provide default initializer forms to determine the value of a form if none is provided:

```
>(def-class person name age :initform (+ 10 1))
t
>(setf p (make-person :name "John"))
#S(...)
>(person-age p)
11
>(person-name p)
"John"
```

## Multiple Inheritance
The *def-class* macro also allows you to inherit from multiple classes:

```
>(def-class thing)
t
>(def-class being age)
t
>(def-class (person being thing) name)
t
>(setf p (make-person :name "John" :age "22"))
#S(...)
>(person-age p)
11
>(being-age p)
11
>(person-name p)
"John"
>(person? p)
t
>(being? p)
t
```
