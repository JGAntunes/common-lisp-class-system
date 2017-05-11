(defmacro def-class (class-name &rest properties)
  `(let* ((constructor-name (concatenate 'string "make-" (string-downcase ',class-name)))
	  (getter-names (mapcar #'(lambda (prop)
                     (concatenate 'string
                             (string-downcase ',class-name)
                             "-"
                             (string-downcase prop)))
                      ',properties))
        (recognizer-name (concatenate 'string (string-downcase ',class-name) "?")))
    
    ;; Defun constructor. 
    ;; It creates a pair, where the first element is the class name
    ;; and the second element is a vector of the properties in order
    (setf (symbol-function (read-from-string constructor-name))
        #'(lambda (&key ,@properties)
          (cons ',class-name (vector ,@properties))))

    ;; Defun getters.
    (loop for getter-name in getter-names
        for i from 0 do
      (setf (symbol-function (read-from-string getter-name))
        (let ((pos i))
          #'(lambda (,class-name)
            (elt (cdr ,class-name) pos)))))

    ;; Defun recognizer
    (setf (symbol-function (read-from-string recognizer-name))
      #'(lambda (instance)
        (and (consp instance)
           (eq (car instance) ',class-name))))

    t))
