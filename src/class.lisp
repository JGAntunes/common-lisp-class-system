;;; Def global class definitions
(defvar *class-definitions* (make-hash-table :test 'equal))

;;; Helper functions

;; Given a class name crawl the class definitions and get all the super classes
(defun crawl-hierarchy (class-name)
  (let ((super-classes (car (gethash class-name *class-definitions*))))
    (append super-classes
          (mapcan #'crawl-hierarchy super-classes))))

(defun generate-getter-names (class-name props)
  (mapcar #'(lambda (prop)
    (concatenate 'string class-name "-" prop))
    props))

(defmacro def-class (classes &rest properties)
   `(let* ((super-classes (if (listp ',classes) (mapcar #'string-downcase (rest ',classes)) ()))
        (class-name (string-downcase (if (listp ',classes) (car ',classes) ',classes)))
        (constructor-name (concatenate 'string "make-" class-name))
        (properties (mapcar #'string-downcase ',properties))
	      (this-getter-names (generate-getter-names class-name properties))
        (recognizer-name (concatenate 'string class-name "?")))

        ;; Set class definition
        (setf (gethash class-name *class-definitions*) (list super-classes properties))

        ;; Defun constructor.
        ;; It creates a hashtable, where the class names of the instance are the keys
        ;; and the second values are vectors of the properties for each class in order
        (setf (symbol-function (read-from-string constructor-name))
            #'(lambda (&rest key-pairs &key ,@properties &allow-other-keys)
               (let* ((instance (make-hash-table :test 'equal))
                      (super (mapcar
                               #'(lambda (class) 
                                    (apply 
                                      (intern (concatenate 'string "MAKE-" (string-upcase class)))
                                      key-pairs))
                               super-classes)))
                      (loop for super-class in super do 
                        (maphash #'(lambda (class props)
                          (setf (gethash class instance) props)) super-class))
                      (setf (gethash class-name instance) (vector ,@properties))
                      instance
                 )))

        ;; Defun `this` getters.
        (loop for getter-name in this-getter-names
            for i from 0 do
          (setf (symbol-function (read-from-string getter-name))
            (let ((pos i))
              #'(lambda (instance)
                (elt (gethash class-name instance) pos)))))
        
        ;; Defun `super` getters.
        (mapcar #'(lambda (super-class)
            (loop with super-getter-names = (generate-getter-names
                                              class-name
                                              (car (rest (gethash super-class *class-definitions*))))
              for i from 0 to (- (list-length super-getter-names) 1) do
            (setf (symbol-function (read-from-string (nth i super-getter-names)))
              (let ((pos i))
                #'(lambda (instance)
                  (elt (gethash super-class instance) pos)))))) (crawl-hierarchy class-name))

        ;; Defun recognizer
        (setf (symbol-function (read-from-string recognizer-name))
          #'(lambda (instance) 
            (and (typep instance 'HASH-TABLE) (if (gethash class-name instance) t NIL))))
    t))
