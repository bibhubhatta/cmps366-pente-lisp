;;;; ******************************************************************
;;;; Serialization lists
;;;; Used for debugging and final demo
;;;; ******************************************************************

(defun case_1 ()
  '(
      ; Board:
      (
      ( O O O O O O O O O O O O B O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O W O O O O O O O O O O O O O O O O )
      ( O O W O O O O O O O O O O O O O O O O )
      ( O O B O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O W W B O O O O O O )
      ( O O O O O O O O O W O O O O O O O O O )
      ( O O O O O O O O O W O O O O O O O O O )
      ( O O O O O O O B O B O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O B O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O B O O O O O O O O O O O O O O O O )
      ( O O W O O O O O O O O O O O O O O O O )
      ( O O W O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      )

      ; Human
      0 0

      ; Computer
      0 0

      ; Next player
      Human Black
  ) 
)


(defun case_2 ()
  '(
      ; Board:
      (
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O W W B O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O B O O O O )
      ( O O O O O B O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O B O O O O O O O )
      ( O O O O O O O O O O W O O O O O O O O )
      ( O O O O O O O O O W O O O O O O O O O )
      ( O O O O O O W W O O O O O O O O O O O )
      ( O O O O O O O O O W O O O O O O O O O )
      ( O O O O O O O O O O W O O O O O O O O )
      ( O O B O O O O O O O O B O O O O O O O )
      ( O O O O O B O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O B O O O )
      ( O O O W W B O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      )

      ; Human
      0 0

      ; Computer
      0 0

      ; Next player
      Computer Black   
  )
)

( defun case_3 ()
  '(
    ; Board:
    (
      ( O O O W W B O O O O O O O O O O O O O )
      ( O O W O O B O O O O O O O B B O O O O )
      ( O O W O O O O O O O O O O O O O O O O )
      ( O O B O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O B O O O O O )
      ( O O O W O O O O O O O O O O O O O O O )
      ( O O O O W O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O W O O O O O O O O O O O O )
      ( O O O O O O O W O W O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O B B O O B B O O O O O O O O )
      ( O O O O O W O O O O O O O O O O O O O )
      ( O O O O O W O O O O O O O O B O O O O )
      ( O O B W W O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
    )

    ; Human
    1 0

    ; Computer
    0 0

    ; Next player
    Human Black
  )
)

(defun case_4 ()
  '(
    ; Board:
    (
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O B B O O O O O O O O O O O O O O )
      ( O O O B O B O O O O O O O O O O O O O )
      ( O O O W O O W O O O O O O O O O O O O )
      ( O O O O O B O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O W O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O W O O O O O O O O O )
      ( O O O O O O O O W O O O O O O O O O O )
      ( O O O O O O O W O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O O O O O )
      ( O O O O O O O O O O O O O O O B O O O )
      ( O O O O O O O O O O O O O O O B O O O )
      ( O O O O W W B O O O O O O O O O O O O )
      ( O O O W O O O O O O O O O O O O O O O )
      ( O O O W O O O O O O O O O O O O O O O )
      ( O O O B O O O O O O O O O O O O O O O )
    )

    ; Human
    3 6

    ; Computer
    0 2

    ; Next player
    Computer White
  )
)

;;;; ******************************************************************
;;;; End of serialization lists
;;;; ******************************************************************


;;;; ******************************************************************
;;;; Board related functions
;;;; ******************************************************************


;;; *********************************************
;;; Name   : board
;;; Purpose: Get the board from the game state
;;; Return : The board -- a list of lists
;;; *********************************************
(defun board (state)
  (nth 0 state)
)


;;; *********************************************
;;; Name   : mark_rows
;;; Purpose: Mark the rows of the board
;;; Return : The board -- a list of lists
;;; *********************************************
(defun mark_rows (unmarked_board)

  (cond 
        ((null unmarked_board) nil)
        (t (cons 
                    ; Mark the first element of the list with the length of the list
                    (cons 
                           (length unmarked_board)
                           (car unmarked_board)
                    ) 
                    ; Recursively mark the rest of the list and cons the result
                    (mark_rows (cdr unmarked_board))
           )
        )
        
  )

)

;;; *********************************************
;;; Name   : range
;;; Purpose: Get a list of numbers from start 
;;;          to end (start inclusive, end exclusive)
;;; Return : The list of numbers
;;; *********************************************

(defun range (start end)
  (cond
        ; Since the range is exclusive of the end,
        ; we have to check if the start + 1 is greater
        ; than or equal to the end
        ((>= (+ 1 start) end) nil)
        (t (cons start (range (+ 1 start) end)))
  )
)


;;; *********************************************
;;; Name   : num_list_to_char_list
;;; Purpose: Convert a list of numbers to a list
;;;          of characters
;;; Return : The list of characters
;;; *********************************************
(defun num_list_to_char_list (num_list)

  (cond
        ((null num_list) nil)
        (t (cons (code-char (+ 64 (car num_list))) (num_list_to_char_list (cdr num_list))))
  )

)


;;; *********************************************
;;; Name   : mark_columns
;;; Purpose: Mark the columns of the board
;;; Return : The board -- a list of lists
;;; *********************************************
(defun mark_columns (unmarked_board)
  
    (append
            
             unmarked_board
             
             
             (list (num_list_to_char_list (range 0 (+ 1 (length (car unmarked_board))))))
            

    )
)


;;; *********************************************
;;; Name   : cartesian_board
;;; Purpose: Get board with position indicators
;;;          for each row and column
;;; Return : The board -- a list of lists
;;; *********************************************
(defun cartesian_board (unmarked_board)

  (mark_columns
                (
                  mark_rows unmarked_board
                )
  )

)


;;;; ******************************************************************
;;;; End of board related functions
;;;; ******************************************************************


(
  print
        (
          cartesian_board
            (
              board
                (
                  case_4
                )
            )
        )
)