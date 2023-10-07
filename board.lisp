;;;; ******************************************************************
;;;; Board related functions
;;;; ******************************************************************

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

            ; if column is negative, return nil
            ; this is to enable compatibility with
            ; functions that recursively call get_stone
            ; and decrement the column char to find 
            ; the end of the board
            ((< (char-code column) 65) nil)
            
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


;;; *********************************************
;;; Name   : top_right_position
;;; Args   : position
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the top right position of the position
;;; Return : The top right position -- a string
;;; *********************************************
(defun top_right_position (position)

  ; use format to concatenate the column char and the row number
  (format nil "~a~a" 
          ; get the next column char
          (code-char (+ 1 (char-code (column_char_from_position position)))) 
          ; get the next row number
          (write-to-string (+ 1 (row_number_from_position position)))
  )
)

;;; *********************************************
;;; Name   : bottom_left_position
;;; Args   : position
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the bottom left position of the position
;;; Return : The bottom left position -- a string
;;; *********************************************
(defun bottom_left_position (position)

  ; use format to concatenate the column char and the row number
  (format nil "~a~a" 
          ; get the previous column char
          (code-char (- (char-code (column_char_from_position position)) 1)) 
          ; get the previous row number
          (write-to-string (- (row_number_from_position position) 1))
  )
)

;;; *********************************************
;;; Name   : top_left_position
;;; Args   : position
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the top left position of the position
;;; Return : The top left position -- a string
;;; *********************************************
(defun top_left_position (position)

  ; use format to concatenate the column char and the row number
  (format nil "~a~a" 
          ; get the previous column char
          (code-char (- (char-code (column_char_from_position position)) 1)) 
          ; get the next row number
          (write-to-string (+ 1 (row_number_from_position position)))
  )
)


;;; *********************************************
;;; Name   : bottom_right_position
;;; Args   : position
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the bottom right position of the position
;;; Return : The bottom right position -- a string
;;; *********************************************
(defun bottom_right_position (position)

  ; use format to concatenate the column char and the row number
  (format nil "~a~a" 
          ; get the next column char
          (code-char (+ 1 (char-code (column_char_from_position position)))) 
          ; get the previous row number
          (write-to-string (- (row_number_from_position position) 1))
  )
)



;;; *********************************************
;;; Name   : get_top_right_diagonal
;;; Args   : board, position
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the top right diagonal of the board
;;; Return : The top right diagonal -- a list
;;; Algo   : Recursively append the top right diagonal
;;;          until the end of the board is reached
;;; *********************************************
(defun get_top_right_diagonal (board position)

  (cond   
            ; if there are no more top right positions,
            ; return nil
            (
               (null 
                     (get_stone board (top_right_position position) ) )
                nil
            )

            ; otherwise, append the top right position
            ; and recursively get the rest of the top right
            ; positions
            (t (cons 
                        (get_stone board (top_right_position position))
                        (get_top_right_diagonal board (top_right_position position))
                )
            )

         
  )

)

;;; *********************************************
;;; Name   : get_bottom_left_diagonal
;;; Args   : board, position
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the bottom left diagonal of the board
;;; Return : The bottom left diagonal -- a list
;;; Algo   : Recursively append the bottom left diagonal
;;;          until the end of the board is reached
;;; *********************************************
(defun get_bottom_left_diagonal (board position)

  (cond   
            ; if there are no more bottom left positions,
            ; return nil
            (
               (null 
                     (get_stone board (bottom_left_position position) ) )
                nil
            )

            ; otherwise, append the bottom left position
            ; and recursively get the rest of the bottom left
            ; positions
            (t (cons 
                        (get_stone board (bottom_left_position position))
                        (get_bottom_left_diagonal board (bottom_left_position position))
                )
            )

         
  )

)

;;; *********************************************
;;; Name   : top_left_diagonal
;;; Args   : board, position
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the top left diagonal of the board
;;; Return : The top left diagonal -- a list
;;; Algo   : Recursively append the top left diagonal
;;;          until the end of the board is reached
;;; *********************************************
(defun get_top_left_diagonal (board position)

  (cond   
            ; if there are no more top left positions,
            ; return nil
            (
               (null 
                     (get_stone board (top_left_position position) ) )
                nil
            )

            ; otherwise, append the top left position
            ; and recursively get the rest of the top left
            ; positions
            (t (cons 
                        (get_stone board (top_left_position position))
                        (get_top_left_diagonal board (top_left_position position))
                )
            )

         
  )

)


;;; *********************************************
;;; Name   : get_bottom_right_diagonal
;;; Args   : board, position
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the bottom right diagonal of the board
;;; Return : The bottom right diagonal -- a list
;;; Algo   : Recursively append the bottom right diagonal
;;;          until the end of the board is reached
;;; *********************************************
(defun get_bottom_right_diagonal (board position)

  (cond   
            ; if there are no more bottom right positions,
            ; return nil
            (
               (null 
                     (get_stone board (bottom_right_position position) ) )
                nil
            )

            ; otherwise, append the bottom right position
            ; and recursively get the rest of the bottom right
            ; positions
            (t (cons 
                        (get_stone board (bottom_right_position position))
                        (get_bottom_right_diagonal board (bottom_right_position position))
                )
            )

         
  )

)



;;; *********************************************
;;; Name   : get_positive_diagonal
;;; Args   : board, position
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the positive diagonal of the board
;;; Return : The positive diagonal -- a list
;;; Algo   : Appends the top right part and the
;;;          bottom left part of the positive diagonal
;;; *********************************************
(defun get_positive_diagonal (board position)

  (
    append 
          ; the bottom left part needs to be reversed
          ; because the order of the stones is reversed
          ; when getting the bottom left diagonal
          (reverse (get_bottom_left_diagonal board position))
           (list (get_stone board position))
           (get_top_right_diagonal board position) 
          
  )
  
)

;;; *********************************************
;;; Name   : get_negative_diagonal
;;; Args   : board, position
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the negative diagonal of the board
;;; Return : The negative diagonal -- a list
;;; Algo   : Appends the top left part and the
;;;          bottom right part of the negative diagonal
;;; *********************************************

(defun get_negative_diagonal (board position)

  (
    append 
          ; the bottom right part needs to be reversed
          ; because the order of the stones is reversed
          (reverse (get_top_left_diagonal board position))
          (list (get_stone board position))
          (get_bottom_right_diagonal board position) 
          
  )
  
)


;;; *********************************************
;;; Name   : row_number_from_position
;;; Args   : position
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the row number from the position
;;; Return : The row number
;;; *********************************************
(defun row_number_from_position (position)
  (parse-integer (subseq position 1))
)

;;; *********************************************
;;; Name   : column_char_from_position
;;; Args   : position
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the column number from the position
;;; Return : The column number
;;; *********************************************
(defun column_char_from_position (position)
  (char-upcase (char (subseq position 0 1) 0))
)

;;; *********************************************
;;; Name   : column_number_from_position
;;; Args   : position
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the column number from the position
;;; Return : The column number
;;; *********************************************
(defun column_number_from_position (position)
  (- (char-code (column_char_from_position position)) 64)
)


;;; *********************************************
;;; Name   : get_stone
;;; Args   : board, position
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          position is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the stone at the position
;;; Return : The stone -- O, W, B, or nil
;;; *********************************************
(defun get_stone (board position)

  (cond
        ; if the board is empty, return nil
        ((null board) nil)

        ; if the board is not empty, get the row
        ; and then get the column of the row
        ; and then get the stone at the position
        ; in the column
        (t (get_row

              ; Get the column
              (get_column
                    board
                    ; Extract the column char from the position
                    (column_char_from_position position)
              )

              ; Extract the row number from the position
              (row_number_from_position position)
           )
        )
  )

)

;;; *********************************************
;;; Name   : replace_row
;;; Args   : board, row_num, new_row
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          row_num is the number that represents
;;;          the row in the board and as shown in
;;;          the printed board
;;;          new_row is the new row to replace the
;;;          old row
;;; Purpose: Replace the row of the board
;;; Return : The board -- a list of lists
;;; *********************************************
(defun replace_row (board row_num new_row)

  (cond
        ((null board) nil)
        ((< (length board) row_num) nil)

        ; if the length of the board is equal to the row number,
        ; then the row is the first element of the board
        ((= (length board) row_num) (cons new_row (cdr board)))

        ; otherwise, continue to replace the row
        (t (cons (car board) (replace_row (cdr board) row_num new_row)))
  )

)

;;; *********************************************
;;; Name   : replace_stone_in_row
;;; Args   : row, column, stone
;;;          row is a list
;;;          column is the number that represents
;;;          the column in the board and as shown in
;;;          the printed board
;;;          stone is the stone to be set at the position
;;;          it is either O, W, or B
;;; Purpose: Replace the stone in the row
;;; Return : The row -- a list
;;; *********************************************
(defun replace_stone_in_row (row column stone)

  (cond
        ((null row) nil)

      
        (
          ; if the column is the first
          (= 0 (- (char-code column) 65)) 
          ; then the stone is the first element of the row
          (cons stone (cdr row))
        
        )

        (
          ; otherwise, continue to replace the stone
          t (cons (car row) (replace_stone_in_row 
                                  (cdr row) 
                                  ; decrement the column char
                                  (code-char (- (char-code column) 1))
                                  stone
                            )
            )
        )
  )

)


;;; *********************************************
;;; Name   : set_stone
;;; Args   : board, position, stone
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          position is a string that represents
;;;          the position of the stone in the board
;;;          stone is the stone to be set at the position
;;;          it is either O, W, or B
;;; Purpose: Set the stone at the position
;;; Return : The board -- a list of lists
;;; *********************************************
(defun set_stone (board position stone)
  
  (

    ; get the row and replace the the stone at the position
    ; in the row
    
      replace_row 

                  board
                  (row_number_from_position position)

                  (
                    replace_stone_in_row
                    (get_row board (row_number_from_position position))
                    (column_char_from_position position)
                    stone
                  )      
  )

)

;;; *********************************************
;;; Name   : convert_to_sequences
;;; Args   : lst
;;;          lst is a list of stones
;;; Purpose: Convert the list of stones to a list
;;;          of sequences
;;; Return : The list of sequences (a list of lists)
;;; *********************************************
(defun convert_to_sequences (lst)
  (cond

      ((null lst) (list nil))

      ((null (cdr lst)) (list (list (car lst))))


      ; if the first element of the list is also the
      ; second element of the list,
      ; then cons the first element to the sequence
      ((equal 
              ; first element of the list
              (car lst) 
              ; second element of the list
              (cadr lst)
       ) 

       ; cons the first element to the sequence inside the list
        (cons

        ; cons the item to the first element of the sequence
        (cons (car lst) (car (convert_to_sequences (cdr lst))))

        ; cons the rest of the list to the rest of the sequences
        (cdr (convert_to_sequences (cdr lst)))
        
        )
      )

      ; otherwise, cons the first element as a new list
      (t (cons (list (car lst)) (convert_to_sequences (cdr lst))))
      
  )

)

;;; *********************************************
;;; Name   : unravel
;;; Args   : lst
;;;          lst can be a nested list of any depth
;;; Purpose: Unravel the nested list to a list
;;; Return : The list
;;; *********************************************
(defun unravel (lst)
  (
    cond
      ((null lst) nil)
      ((atom lst) (list lst))
      (t (append (unravel (car lst)) (unravel (cdr lst))))
  )
)

;;; *********************************************
;;; Name   : get_empty_positions
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get the empty positions of the board
;;; Return : The empty positions -- a list of position
;;;          strings
;;; *********************************************
(defun get_empty_positions (board)
  (cond
        ((null board) nil)
        (t (append 
                (get_empty_positions (cdr board))
                (get_empty_positions_in_row (car board) (length board))
           )
        )
  )
)

;;; *********************************************
;;; Name   : get_empty_positions_in_row
;;; Args   : row, row_num
;;;          row is a list
;;;          row_num is the number that represents
;;;          the row in the board and as shown in
;;;          the printed board
;;; Purpose: Get the empty positions in the row
;;; Return : The empty positions -- a list of position
;;;          strings
;;; *********************************************
(defun get_empty_positions_in_row (row row_num)
  (cond
        ((null row) nil)
       
        (
          ; row needs to be reversed because,
          ; to deduce the column char, we need
          ; to get the length of the row
          (equal (car (reverse row)) 'O) 
          
          (
            cons 
                  (format nil "~a~a" 
                          (code-char (+ 64 (length row))) 
                          (write-to-string row_num)
                  )
                  
                  (get_empty_positions_in_row 
                                ; remove the last element of the row
                                ; and get the rest of the empty positions
                                (reverse (cdr (reverse row))) 
                                row_num
                  )
          )
        )
        
        (t (get_empty_positions_in_row (reverse (cdr (reverse row))) row_num))
  )
)


;;; *********************************************
;;; Name   : get_no_stones_on_board
;;; Args   : board, stone
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          stone is the stone to be checked
;;;          it is either O, W, or B
;;; Purpose: Get the number of stones on the board
;;; Return : The number of stones
;;; *********************************************
(  defun get_no_stones_on_board  (board stone)
    (
      count 
            stone 
            (unravel board)
    )
)

;;; *********************************************
;;; Name   : remove_first_column
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Remove the first column of the board
;;; Return : The board -- a list of lists
;;; *********************************************
(defun remove_first_column (board)
  (
    cond
      ((null board) nil)
      (t (cons (cdr (car board)) (remove_first_column (cdr board))))
  )
)

;;; *********************************************
;;; Name   : get_first_column
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get the first column of the board
;;; Return : The first column -- a list
;;; *********************************************
(defun get_first_column (board)
  (
    cond
      ((null board) nil)
      (t (cons (caar board) (get_first_column (cdr board))))
  )
)


;;; *********************************************
;;; Name   : get_all_board_columns
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get all the columns of the board
;;; Return : The columns -- a list of lists
;;; *********************************************
(defun get_all_board_columns (board)
  
  (
    cond
      ((null (caar board)) nil)
      (t (cons 
              (get_first_column board)
              (get_all_board_columns (remove_first_column board))
         )
      )
  )
)


;;; *********************************************
;;; Name   : get_first_row_positions
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get all the positions of the first row
;;; Return : The positions -- a list of position
;;;          strings
;;; Algo   : Recursively append the positions
;;;          until the end of the board is reached
;;;          while removing the first row every time
;;; *********************************************
(defun get_first_row_positions (board)
    (
      cond
            ((null board) nil)
            (
              t (cons 
                      (format nil "~a~a" 
                              (code-char (+ 64 (length board))) 
                              (write-to-string 1)
                      )
                      (get_first_row_positions (cdr board))
                 )
            
            )
    )

)

;;; *********************************************
;;; Name   : get_first_column_positions
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get all the positions of the first column
;;; Return : The positions -- a list of position
;;;          strings
;;; Algo   : Recursively append the positions
;;;          until the end of the board is reached
;;;          while removing the first column every time
;;; *********************************************
(defun get_first_column_positions (board)
    (
      cond
            ((null (caar board)) nil)
            (
              t (cons 
                      (format nil "~a~a" 
                              (code-char (+ 64 1)) 
                              (write-to-string (length (car board)))
                      )
                      (get_first_column_positions (remove_first_column board))
                 )
            
            )
    )

)

;;; *********************************************
;;; Name   : get_all_diagonal_starts
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get all the diagonal starts of the board
;;; Return : The diagonal starts -- a list of position
;;;          strings
;;; Algo   : Recursively append the diagonal starts
;;;          until the end of the board is reached
;;;          and remove the duplicates
;;;          The diagonal starts are the cells
;;;          of the first row and column of the board
;;; *********************************************
(defun get_all_diagonal_starts (board)
  (
    cond
        ((null board) nil)
        (
          t 
            (
              ; duplicates need to be removed because
              ; the first cell is in both the first 
              ; row and the first column of the board
              remove-duplicates
              (
                append
                      (get_first_row_positions board)
                      (get_first_column_positions board)
              )
              :test #'equal

            )
        )


  )
)

;;; *********************************************
;;; Name   : get_diagonals_from_positions
;;; Args   : board, positions
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          positions is a list of position strings
;;; Purpose: Get the diagonals from the positions
;;; Return : The diagonals -- a list of lists
;;; Algo   : Recursively append the diagonals
;;;          until the end of the positions is reached
;;; *********************************************
(defun get_diagonals_from_positions (board positions)
  (
    cond
        ((null positions) nil)
        (
          t 
            (
              append 
                    (list (get_positive_diagonal board (car positions)))
                    (list (get_negative_diagonal board (car positions)))
                    (get_diagonals_from_positions board (cdr positions))
            )
        )
    
   )
)

;;; *********************************************
;;; Name   : get_all_diagonals
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get all the diagonals of the board
;;; Return : The diagonals -- a list of lists
;;; *********************************************
(defun get_all_diagonals (board)

  (
    get_diagonals_from_positions board (get_all_diagonal_starts board)
  )
  
)

;;; *********************************************
;;; Name   : get_all_board_sequences
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get all the sequences of the board
;;; Return : The sequences -- a list of lists
;;; *********************************************
(defun get_all_board_sequences (board)
  (
    append 
          ; board is represented as a list of boards
          board
          (get_all_board_columns board)
          (get_all_diagonals board)
  )
)


;;; *********************************************
;;; Name   : convert_board_sequences_to_stone_sequences
;;; Args   : board, board_sequences
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          board_sequences is a list of lists
;;;          that represent the sequences of the board
;;;          e.g. rows, columns, diagonals
;;; Purpose: Convert the board sequences to stone sequences
;;; Return : The stone sequences -- a list of lists
;;; *********************************************
(defun convert_board_sequences_to_stone_sequences (board board_sequences)
  (
    cond
        ((null board_sequences) nil)
        (
          t 
            (
              append 
                    (convert_to_sequences (car board_sequences))
                    (convert_board_sequences_to_stone_sequences board (cdr board_sequences))
            )
        )
    
   )
)

;;; *********************************************
;;; Name   : get_all_stone_sequences
;;; Args   : board, stone
;;;          board is a list of lists
;;;          without the row/column markers,
;;;          stone is the stone to be checked
;;;          it is either O, W, or B
;;; Purpose: Get all the sequences of the board
;;;          that contain the stone
;;; Return : The sequences -- a list of lists
;;; Algo   : Get all the sequences of the board
;;;          and then convert the sequences to stone
;;;          sequences, then filter the stone sequences
;;;          to get only the sequences that contain
;;;          the stone
;;; *********************************************
(defun get_all_stone_sequences (board stone)

    ; filter the stone sequences to get only the sequences
    ; that contain the stone
    (remove-if-not
      
      #'(lambda (stone_sequence)
          (member stone stone_sequence)
        )

      (
        convert_board_sequences_to_stone_sequences 
        board 
        (get_all_board_sequences board)
      )
    )
  
)

;;; *********************************************
;;; Name   : get_distance
;;; Args   : position_1, position_2
;;;          position_1 is a string that represents
;;;          the position of the stone in the board
;;;          position_2 is a string that represents
;;;          the position of the stone in the board
;;; Purpose: Get the distance between two positions
;;; Return : The distance -- a number
;;; *********************************************
(defun get_distance (postion_1 position_2)

  (max
      (abs (- (row_number_from_position postion_1) (row_number_from_position position_2)))
      (abs (- (column_number_from_position postion_1) (column_number_from_position position_2)))
  )

)

;;; *********************************************
;;; Name   : get_empty_board
;;; Args   : no_rows, no_column
;;;          no_rows is the number of rows of the board
;;;          no_column is the number of columns of the board
;;; Purpose: Get an empty board
;;; Return : The empty board -- a list of lists
;;; *********************************************
(defun get_empty_board (no_rows no_column)
  (
    cond
        ((<= no_rows 0) nil)
        (t (cons (get_empty_row no_column) (get_empty_board (- no_rows 1) no_column)))
  )
)


;;; *********************************************
;;; Name   : get_empty_row
;;; Args   : no_columns
;;;          no_columns is the number of columns of the board
;;; Purpose: Get an empty row
;;; Return : The empty row -- a list
;;; *********************************************

(defun get_empty_row (no_columns)
  (
    cond
        ((<= no_columns 0) nil)
        (t (cons 'O (get_empty_row (- no_columns 1))))
  )
)


;;; *********************************************
;;; Name   : get_board_shape
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get the shape of the board
;;; Return : The shape -- a list containing the
;;;          number of rows and columns
(defun get_board_shape (board)
  (list (length board) (length (car board)))
)

;;; *********************************************
;;; Name   : get_no_rows
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get the number of rows of the board
;;; Return : The number of rows
;;; *********************************************
(defun get_no_rows (board)
  (length board)
)

;;; *********************************************
;;; Name   : get_no_columns
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get the number of columns of the board
;;; Return : The number of columns
;;; *********************************************
(defun get_no_columns (board)
  (length (car board))
)

;;; *********************************************
;;; Name   : get_center
;;; Args   : board
;;;          board is a list of lists
;;;          without the row/column markers,
;;; Purpose: Get the center position of the board
;;; Return : The center position -- a string
;;; *********************************************
(defun get_center (board)
  (format nil "~a~a" 
          ; Using ceiling instead of / so that it also 
          ; works for board with even number of rows/columns
          (code-char (+ 64 (ceiling (+ 1 (get_no_columns board)) 2)))
          (write-to-string (ceiling (+ 1 (get_no_rows board)) 2))
  )
)

;;;; ******************************************************************
;;;; End of board related functions
;;;; ******************************************************************