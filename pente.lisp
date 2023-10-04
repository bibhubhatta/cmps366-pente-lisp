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
(defun get_board (state)
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

;;; *********************************************
;;; Name   : print_board_cell
;;; Purpose: Print a cell of the board
;;; Return : nil
;;; *********************************************
(defun print_board_cell (cell)
  (cond
        ; if the cell is a number, 
        ; it is a row marker, so print it
        ; as two digits for alignment
        ((numberp cell)  (format t "~2d" cell))
        
        
        ; ((char-equal cell #\@) (princ "  "))

        ; if the cell is a character 
        ; it is a column marker
        ((characterp cell) 
        
          (cond
                ; if the cell is an @, it is the
                ; beginning of a row marker, so print
                ; two spaces for alignment
                ((char-equal cell #\@) (princ "  "))

                ; otherwise, print the character
                (t (princ cell))
          )
        )

        ; otherwise, it is a stone/empty cell, so print it
        (t (princ cell))
  )

  ; print a space for readability
  (princ " ")
)

;;; *********************************************
;;; Name   : print_row
;;; Purpose: Print a row of the board
;;; Return : nil
;;; *********************************************
(
  defun print_row (row)
    (
      cond
        
        ((null row) 
                    ; print a newline to separate rows
                    (terpri)
        )

        ( t
            (print_board_cell (car row))
            (print_row (cdr row))  
        )
    )
    
)

;;; *********************************************
;;; Name   : print_board
;;; Purpose: Print the board
;;; Return : nil
;;; *********************************************
(defun print_board (board)
  (
    cond
      ((null board) nil)
      ( t
          
            (print_row (car board))
            (print_board (cdr board))
          
      )
  )
  

)

;;; *********************************************
;;; Name   : get_row
;;; Args   : board, row
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          row is the number that represents
;;;          the row in the board and as shown in
;;;          the printed board
;;; Purpose: Get a row of the board
;;; Return : The row
;;; *********************************************
(defun get_row (board row)

  (cond 
        ; if the length of the board is equal to the row,
        ; then the row is the first element of the board
        ; because the board is labeled from bottom to top
        ((= (length board) row) (car board))

        ((< (length board) row) nil)

        (t (get_row (cdr board) row))
           
  )

)

;;; *********************************************
;;; Name   : get_column
;;; Args   : board, column
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          column is the character that represents
;;;          the column in the board and as shown in
;;;          the printed board
;;; Purpose: Get a column of the board
;;; Return : The column -- a list
;;; *********************************************
(defun get_column (board column)

    (
      cond
            ((null board) nil)
            
            (t (cons 
                        ; get the nth column of the board
                        (nth (- (char-code column) 65) (car board))
                        ; and recursively get the rest of the columns
                        (get_column (cdr board) column)
                        ; and cons the result
                )    
            )
    )

)


;;;; ******************************************************************
;;;; End of board related functions
;;;; ******************************************************************


(
  print_board
        (
          cartesian_board
            (
              get_board
                (
                  case_4
                )
            )
        )
)


(
  print_row
    (
      get_row
        (
          get_board
            (
              case_4
            )
        )

        16
    )

)

; (trace get_column)

(
  print (
          get_column
            (
              get_board
                (
                  case_4
                )
            )
            #\F
        )
)