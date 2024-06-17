# A* Search Algorithm for Sliding Puzzle

## Overview
This code implements the A* search algorithm to solve the 8-puzzle problem (sliding puzzle). It represents the state of the board, calculates A* values, and finds the solution path from the initial state to the goal state.

## Data Structures

### `state` Struct
The `state` struct represents the state of the board and includes the following fields:
- `board`: The current configuration of the board.
- `g`: The cost to move from the start state to the current state.
- `h`: The heuristic value, which is the number of misplaced tiles compared to the goal state.
- `f`: The total cost function, calculated as `f(n) = g(n) + h(n)`.
- `parent`: The previous state that led to the current state.

## Global Variables
- `*expanded-nodes*`: A counter to keep track of the number of nodes expanded during the search.
- `*goal-state*`: The final state of the board that we want to achieve.

## Functions

### `misplaced-tiles`
Calculates the heuristic value `h(n)` as the number of tiles misplaced compared to the goal state.
```lisp
(defun misplaced-tiles (board)
    (let ((count 0))
        (dotimes (i 3)
        (dotimes (j 3)
            (unless (or (and (= i 2) (= j 2))
                        (eql (nth j (nth i board)) (nth j (nth i *goal-state*))))
                (incf count))))
            count))
```

### `move`
Moves the blank tile ('E') in the specified direction (`:up`, `:down`, `:left`, `:right`) and returns the new board configuration.
```lisp
(defun move (board dir)
    (let ((blank-pos (position 'E (apply #'append board))))
        (multiple-value-bind (x y) (floor blank-pos 3)
            (case dir
                (:up (if (> x 0) (swap board x y (- x 1) y)))
                (:down (if (< x 2) (swap board x y (+ x 1) y)))
                (:left (if (> y 0) (swap board x y x (- y 1))))
                (:right (if (< y 2) (swap board x y x (+ y 1))))))))
```

### `swap`
Helper function to swap tiles on the board.
```lisp
(defun swap (board x1 y1 x2 y2)
    (let ((new-board (copy-seq (map 'list #'copy-seq board))))
        (setf (nth y1 (nth x1 new-board)) (nth y2 (nth x2 board))
              (nth y2 (nth x2 new-board)) (nth y1 (nth x1 board)))
            new-board))
```

### `generate-children`
Generates child states for a given state based on possible moves.
```lisp
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
```

### `solve`
A* algorithm to find the solution from the initial state to the goal state.
```lisp
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
```

### `print-path`
Prints the path from the initial state to the goal state.
```lisp
(defun print-path (node)
    (when node
        (print-path (state-parent node))
        (print (state-board node))))
```

### `main`
Entry point for the program. It initializes the initial state, calls the solve function, and prints the result.
```lisp
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
```

## Usage
1. Load the code into your Common Lisp environment.
2. Run the `main` function to start the A* search algorithm.
3. The program will print the solution path and the number of nodes expanded during the search.

This implementation demonstrates the use of the A* algorithm to solve the 8-puzzle problem, providing a clear path from the initial state to the goal state while expanding the minimum number of nodes.
