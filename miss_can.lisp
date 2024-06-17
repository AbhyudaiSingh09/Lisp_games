(defparameter *depth* (parse-integer (or (nth 1 *args*) "40")))
(defparameter *missionaries-count* (parse-integer (or (nth 2 *args*) "15")))
(defparameter *cannibals-count* (parse-integer (or (nth 3 *args*) "15")))

(defun valid-state(m c b)
    "Check for valid state"
    ;;ensure canibals are always more 
    (and (or (>= m c) (eql m 0))
         (<= (+ m c) (* 2 *missionaries-count*)) ;;total people 30 
         (or (>= (- *missionaries-count* m) (- *cannibals-count* c)) (eql (- *missionaries-count* m) 0))))

(defun get-moves (m c b)
    "Generate possible next states given the current state"
    (let ((moves nil)) ;;store moves 
        (dotimes (dm 3) ;; loop for missionaries
            (dotimes (dc 3) ;;loop canibals 
                (when (and (<= 1(+ dm dc) 6) (or (> dm 0) (> dc 0))) ;; check for valid movements 
                    (multiple-value-bind (nm nc nb);; calculate  new moves 
                        (if (eql b 1) ;;is boat on strting side 
                            (values (- m (* dm b)) (- c (* dc b)) (- 1 b)) ;;move from start side to destination 
                            (values (+ m (* dm b)) (+ c (* dc b)) (- 1 b)))  ;;move from  destination side to start 
                    (when (valid-state nm nc nb) ;; if new state valid add to list 
                        (push (list nm nc nb) moves))))))
    moves))

(defun dls (node goal depth)
    "Depth limited searching"
    (format t "Processing state at depth ~A: Missionaries: ~A, Cannibals: ~A, Boat: ~A~%"
            depth (first node) (second node) (if (eql (third node) 1) "Start" "Destination"))
    (if (equal node goal)
        (return-from dls (list node))
        (if (<= depth 0)
            (return-from dls nil)
            (let ((valid-children (remove-if (lambda (x) (member x *visited* :test 'equal))
                                            (get-moves (first node) (second node) (third node)))))
                (push node *visited*)
                (dolist (child valid-children);; recussice search takes a lot of space check 
                    (let ((result (dls child goal (1- depth))))
                        (when result
                            (return-from dls (cons node result)))))))))

            
(defun iddfs (start goal max-depth)
    "ID DFS"
    (loop for depth from 0 to max-depth
        do (progn 
            (format t "Starting search at depth ~A~%" depth)
            (setq *visited* nil) ;;reset the visited search at each level 
            (let ((result (dls start goal depth)))
                (when result
                    (format t "Potential solution found at depth ~A~%" depth)
                    (return result))))))

(defvar *visited* nil) ;; keeps track of visited states during the search 

(defun print-solution ()
    "The solution"
    (let ((solution (iddfs (list *missionaries-count* *cannibals-count* 1) '(0 0 0) *depth*)))
    (if solution 
        (dolist (state solution) ;;print the sate in loop for result 
            (format t "Missionaries: ~A, Cannibals: ~A, Boat: ~A~%"
                    (first state)
                    (second state)
                    (if (eql (third state) 1) "Start" "Destination")))
        (format t "No solution found: ~%"))))

(print-solution) ;;Execute the program 