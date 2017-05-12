# Lisp Class System
This project implements a class system for Lisp through a set of macros. The implemented features are:

* Classes 
* Default Slot Initializers
* Multiple Inheritance
* Methods
* Generics

## Classes
The *def-class* macro allows you to create a class complete with default constructor, getters, setters, recognizers. For instance, to create a 
class *person* with slots *name* and *age* and respective default methods do:

<pre> <code>
<b>> (def-class person name age)</b>
(...)
<b>> (setf p (make-person :age 50 :name John))</b>
#S(...) 
<b>> (person-age p)</b>
50
<b>> (set-person-name p "Bill")</b>
"Bill"
<b>> (person-name p)</b>
"Bill"
<b>> (person? p)</b>
t
</code> </pre>

## Default Slot Initializers
It's possible to provide default initializer forms to determine the value of a form if none is provided:

<pre> <code>
<b>> (def-class person name age :initform (+ 10 1))</b>
t
<b>> (setf p (make-person :name "John"))</b>
#S(...)
<b>> (person-age p)</b>
11
<b>> (person-name p)</b>
"John"
</code> </pre>

## Multiple Inheritance
The *def-class* macro also allows you to inherit from multiple classes:

<pre> <code>
<b>> (def-class thing)</b>
t
<b>> (def-class being age)</b>
t 
<b>> (def-class (person being thing) name)</b>
t
<b>> (setf p (make-person :name "John" :age "22"))</b>
#S(...)
<b>> (person-age p)</b>
11
<b>> (being-age p)</b>
11
<b>> (person-name p)</b>
"John"
<b>> (person? p)</b>
t
<b>> (being? p)</b>
t
</code></pre>
