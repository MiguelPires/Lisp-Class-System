# Lisp Class System
This project implements a class system for Lisp through a set of macros. The implemented features are:

* Classes 
* Default Slot Initializers
* Multiple Inheritance
* Multiple-Dispatch Methods
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

## Generic Functions
The *def-generic* macro allows you specify a function's name and parameters, omitting the implementation. This functionality is complemented by multiple-dispatch methods, described in the next section.

<pre><code>
<b>> (def-generic sum (p1 p2))</b>
SUM
</code></pre>

## Multiple-dispatch Methods
The *def-method* macro allows you specify a generic function's implementation for a specific set of parameter specializers:
<pre><code>
<b>> (def-method sum ((person p1) (person p2)
        (format t "Adding two people's ages")
        (+ (person-age p1) (person-age p2))) </b>
SUM
<b>> (def-method sum ((person p1) (being b2))
        (format t "Adding a person and a being's age")
        (+ (person-age p1) (being-age b2)))</b>
SUM
<b>> (sum (make-person :age 1) (make-person :age 2)) </b>
"Adding two people's ages"
3
<b>> (sum (make-person :age 1) (make-being :age 2)) </b>
"Adding a person and a being's age"
3
</code></pre>
Note that in the first example, both methods would be applicable since *person* is a subclass of *being*. However, the class system invokes the most specific of all the applicable methods. This is the coherent with the standard method combination of the Common Lisp Object System.
