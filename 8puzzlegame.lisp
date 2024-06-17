; struct to represnt the state of the board and it's assocaited A* values 
(defstruct state 
    board ;board configuration
    g ;cost to move form start to current state g(n)
    h ; h(n) value 
    f ; f(n)=h(n) + g(n)
    parent) ;last state 


(defvar *expanded-nodes* 0) ;counter


(defvar *goal-state* '((1 2 3) (4 5 6) (7 8 E))) ;final state 

;mispllaced tiles as compared to goal state 
(defun misplaced-tiles (board)
    (let ((count 0))
        (dotimes (i 3)
        (dotimes (j 3)
            (unless (or (and (= i 2) (= j 2))
                        (eql (nth j (nth i board)) (nth j (nth i *goal-state*))))
                (incf count))))
            count))

;move tile function 
(defun move (board dir)
    (let ((blank-pos (position 'E (apply #'append board))))
        (multiple-value-bind (x y) (floor blank-pos 3)
            (case dir
                (:up (if (> x 0) (swap board x y (- x 1) y)))
                (:down (if (< x 2) (swap board x y (+ x 1) y)))
                (:left (if (> y 0) (swap board x y x (- y 1))))
                (:right (if (< y 2) (swap board x y x (+ y 1))))))))

;helper to move tile on board 
(defun swap (board x1 y1 x2 y2)
    (let ((new-board (copy-seq (map 'list #'copy-seq board))))
        (setf (nth y1 (nth x1 new-board)) (nth y2 (nth x2 board))
              (nth y2 (nth x2 new-board)) (nth y1 (nth x1 board)))
            new-board))

;generate child state for a given state based on possible moves 
(defun generate-children (node)
    (remove-if #'null
        (mapcar (lambda (dir)
                (let ((new-board (move (state-board node) dir)))
                    (if new-board
                        (let ((g (+ 1 (state-g node)))
                                (h (misplaced-tiles new-board)))
                            (make-state :board new-board
                                        :g g
                                        :h h 
                                        :f (+ g h)
                                        :parent node)))))
                '(:up :down :left :right))))

; A* algorithm to solution 
(defun solve (initial-state)
    (let ((open (list initial-state))
            (closed nil))
        (loop
            (if (null open)
                (return-from solve "No solution"))
            (setf open (sort open (lambda (a b) (< (state-f a) (state-f b)))))
            (let ((current (pop open)))
                (if (equal (state-board current) *goal-state*)
                    (return-from solve current))
                (push current closed)
                (incf *expanded-nodes*)
                (dolist (child (generate-children current))
                    (unless (or (some (lambda (node) (equal (state-board node) (state-board child))) open)
                                (some (lambda (node) (equal (state-board node) (state-board child))) closed))  
                        (push child open)))))))

                    
;print the path to initial to goal state in recrusion 
(defun print-path (node)
    (when node
        (print-path (state-parent node))
        (print (state-board node))))

;A* search 
(defun main()
    (let ((initial-state (make-state :board '((E 1 3) (4 2 5) (7 8 6))
                                     :g 0 
                                     :h (misplaced-tiles '((E 1 3) (4 2 5) (7 8 6)))
                                     :f (misplaced-tiles '((E 1 3) (4 2 5) (7 8 6)))
                                     :parent  nil)))
    (setf *expanded-nodes* 0)
    (let ((result (solve initial-state)))
        (if result
            (progn
                (print "Solution Found")
                (print-path result)
                (format t "number of nodes expanded: ~a" *expanded-nodes*))
            (print "No Solution")))))


(main)