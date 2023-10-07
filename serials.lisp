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
