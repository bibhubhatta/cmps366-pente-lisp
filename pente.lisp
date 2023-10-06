(load "board.lisp")
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
;;;; Game state related functions
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
;;; Name   : other_stone
;;; Args   : stone
;;;          stone is the stone of the player
;;; Purpose: Get the stone of the other player
;;; Return : The stone -- W or B or nil if the player
;;;         is invalid
;;; *********************************************

(defun other_stone (stone)
  (cond
    ; handle single character representations
    ((equal stone 'W) 'B)
    ((equal stone 'B) 'W)
    ; handle full word representations
    ((equal stone 'Black) 'White)
    ((equal stone 'White) 'Black)
    (t nil)
  )
)

;;; *********************************************
;;; Name   : other_player
;;; Args   : player
;;;          player is the Human or Computer
;;; Purpose: Get the other player
;;; Return : The other player -- Human or Computer
;;; *********************************************
(defun other_player (player)
  (cond
      ((equal player 'Human) 'Computer)
      ((equal player 'Computer) 'Human)
  )
)


;;; *********************************************
;;; Name   : get_stone_from_player
;;; Args   : game_state, player
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Get the stone that the player is playing
;;; Return : The stone -- W or B or nil if the player
;;;         is invalid
;;; *********************************************
(defun get_stone_from_player (game_state player)

  (cond
    ((equal player (nth 5 game_state)) (nth 6 game_state))
    ((equal (other_player player) (nth 5 game_state)) (other_stone (nth 6 game_state)))
  )

)

;;;; ******************************************************************
;;;; End of game state related functions
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

(
  print 
        (
          get_stone
            (
              get_board
                (
                  case_4
                )
            )
            "F4"
        )
)


(print "Testing set_stone")
(terpri)

(print "Testing get_positive_diagonal with e4")

(
  print 
        (
          get_positive_diagonal
            (
              get_board
                (
                  case_4
                )
            )
            "e4"
        )
)

(terpri)
(terpri)


(print "Testing get_negative_diagonal with g16")

(
  print 
        (
          get_negative_diagonal
            (
              get_board
                (
                  case_4
                )
            )
            "g16"
        )
)

(terpri)
(terpri)

(print "Testing convert_to_sequences with positive diagonal of h8")

; (trace convert_to_sequences)

(
  print 
        (
          convert_to_sequences
            (
              get_positive_diagonal
                (
                  get_board
                    (
                      case_4
                    )
                )
                "h8"
            )
        )
)

(terpri)
(terpri)

(print "Testing unravel")
(
  print 
        (
          unravel
            (
                      get_board
                        (
                          case_4
                        )
            )
        )
)

(terpri)
(terpri)

(print "Testing set_stone at J1, black" )

(
  print_board(
                cartesian_board
                  (
                    set_stone
                      (
                        get_board
                          (
                            case_4
                          )
                      )
                      "j1"
                      'B
                  )
              )
)

(terpri)
(terpri)

(print "Testing get_empty_positions")

(
  print 
        (
          get_empty_positions
            (
              get_board
                (
                  case_4
                )
            )
        )
)

(terpri)
(terpri)

(print "Testing get_no_stones_on_board")
(terpri)

(princ "Number of black stones: ")
(
  princ
        (
          get_no_stones_on_board
            (
              get_board
                (
                  case_4
                )
            )
            'B
        )
)
(terpri)

(princ "Number of white stones: ")
(
  princ 
        (
          get_no_stones_on_board
            (
              get_board
                (
                  case_4
                )
            )
            'W
        )
)
(terpri)

(princ "Number of empty positions: ")
(
  princ
        (
          get_no_stones_on_board
            (
              get_board
                (
                  case_4
                )
            )
            'O
        )
)
(terpri)


(print "Testing get_all_board_columns with case 4")
(terpri)

(
  print
      (
          get_all_board_columns
            (
                get_board
                  (
                      case_4
                  )
            )
      )
     
)

(terpri)
(terpri)

(print "Testing get_all_diagonal_starts with case 4")
(terpri)

(
  print
      (
          get_all_diagonal_starts
            (
                get_board
                  (
                      case_4
                  )
            )
      )
     
)

(terpri)
(terpri)

(print "Testing get_all_diagonals with case 4")
(terpri)

(
  print
      (
          get_all_diagonals
            (
                get_board
                  (
                      case_4
                  )
            )
      )
     
)

(terpri)
(terpri)

(print "Testing get_all_board_sequences with case 4")
(terpri)

(
  print
      (
          get_all_board_sequences
            (
                get_board
                  (
                      case_4
                  )
            )
      )
     
)

(terpri)
(terpri)

(print "Testing get_all_stone_sequences with case 4")
(terpri)

(
  print
      (
          get_all_stone_sequences
            (
                get_board
                  (
                      case_4
                  )
            )
            'W
      )
     
)

(terpri)
(terpri)

(print "Testing get_stone_from_player with cases 3 and 4")
(terpri)

; (trace other_stone)
; (trace other_player)
; (trace get_stone_from_player)

(format
      t
      "Human's stone in case 3: ~a~%"
      (
         get_stone_from_player
            (
              case_3
            )

            'human
      )
)

(format
      t
      "Computer's stone in case 3: ~a~%"
      (
         get_stone_from_player
            (
              case_3
            )

            'computer
      )
)

(format
      t
      "Human's stone in case 4: ~a~%"
      (
         get_stone_from_player
            (
              case_4
            )

            'human
      )
)

(format
      t
      "Computer's stone in case 4: ~a~%"
      (
         get_stone_from_player
            (
              case_4
            )

            'computer
      )
)

(terpri)
(terpri)


(print "Printing board from case 4 for testing")
(terpri)

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



