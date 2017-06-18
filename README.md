# common-lisp-class-system
A Class System for Common Lisp made for AP class @ IST

## Usage

The class definition:
```
(def-class person
  name
  age)
```

An example of its use:
```
> (let ((p (make-person :name "Paulo" :age 33)))
    (person-age p))

33
```

Inheritance:
```
(def-class (student person)
  course)
```

An example of its use:
```
> (let ((s (make-student :name "Paul" :age 21 :course "Informatics")))
    (list (student-name s) (student-course s)))

("Paul" "Informatics")
```

Multiple inheritance:
```
(def-class sportsman
  activity
  schedule)

(def-class (ist-student student sportsman))
```

An example of its use:
```
> (let ((m (make-ist-student :name "Maria" :course "IA" :activity "Tennis")))
    (list (ist-student? m)
      (student? m)
      (sportsman? m)
      (ist-student-name m)
      (person-name m)
      (sportsman-activity m)
      (ist-student-activity m)))

(T T T "Maria" "Maria" "Tennis" "Tennis")
```

## Running it

You can load the load.lisp file and use it accordingly or, if you're using docker, you can do:

Build the container:
```
./build.sh
```

Build and run the container:
```
RUN=1 ./build.sh
```
