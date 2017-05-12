(defmacro def-class (classes &rest properties)
   `(let* ((super-classes (if (listp ',classes) (rest ',classes) ()))
        (class-name (if (listp ',classes) (car ',classes) ',classes))
        (constructor-name (concatenate 'string "make-" (string-downcase class-name)))
	      (getter-names (mapcar #'(lambda (prop)
                     (concatenate 'string
                             (string-downcase class-name)
                             "-"
                             (string-downcase prop)))
                      ',properties))
        (recognizer-name (concatenate 'string (string-downcase class-name) "?")))
        
        ;; Defun constructor.
        ;; It creates a pair, where the first element is the class name
        ;; and the second element is a vector of the properties in order
        (setf (symbol-function (read-from-string constructor-name))
            #'(lambda (&rest key-pairs &key ,@properties &allow-other-keys)
               (let* ((instance (make-hash-table))
                    (super (mapcar #'(lambda (class) 
                      (apply (intern (concatenate 'string "MAKE-" (string-upcase class))) key-pairs)) super-classes)))
                    (loop for super-class in super do 
                      (maphash #'(lambda (class props)
                        (setf (gethash class instance) props)) super-class))
                    (setf (gethash class-name instance) (vector ,@properties))
                    instance
                 )))

        ;; Defun getters.
        (loop for getter-name in getter-names
            for i from 0 do
          (setf (symbol-function (read-from-string getter-name))
            (let ((pos i))
              #'(lambda (instance)
                (elt (gethash class-name instance) pos)))))

        ;; Defun recognizer
        (setf (symbol-function (read-from-string recognizer-name))
          #'(lambda (instance)
            (if (gethash class-name instance) t NIL)))

    t))
