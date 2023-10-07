(load "board.lisp")
(load "serials.lisp")
(load "game_state.lisp")


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

(print "Testing get_player_from_stone with cases 3 and 4")
(terpri)

; (trace get_player_from_stone)


(format 
      t
      "Player of white stone in case 3: ~a~%"
      (
        get_player_from_stone
          (
            case_3
          )
          'W
      )
)


(format 
      t
      "Player of black stone in case 3: ~a~%"
      (
        get_player_from_stone
          (
            case_3
          )
          'B
      )
)

(format 
      t
      "Player of white stone in case 4: ~a~%"
      (
        get_player_from_stone
          (
            case_4
          )
          'W
      )
)

(format 
      t
      "Player of black stone in case 4: ~a~%"
      (
        get_player_from_stone
          (
            case_4
          )
          'B
      )
)

(terpri)
(terpri)

(print "Testing get_score with case 4")
(terpri)

; (trace get_score)

(format
      t
      "Human's score in case 4: ~a~%"
      (
        get_tournament_score
          (
            case_4
          )
          'human
      )
)

(format
      t
      "Computer's score in case 4: ~a~%"
      (
        get_tournament_score
          (
            case_4
          )
          'computer
      )
)

(terpri)
(terpri) 


(print "Testing get_no_captures with case 4")
(terpri)

; (trace get_no_captures)

(format
      t
      "Human's number of captures in case 4: ~a~%"
      (
        get_no_captures
          (
            case_4
          )
          'human
      )
)

(format
      t
      "Computer's number of captures in case 4: ~a~%"
      (
        get_no_captures
          (
            case_4
          )
          'computer
      )
)

(format
  t
  "White's number of captures in case 4: ~a~%"
  (
    get_no_captures
      (
        case_4
      )
      'W
  )
)


(format
  t
  "Black's number of captures in case 4: ~a~%"
  (
    get_no_captures
        (
          case_4
        )

        'B
  )

)

(terpri)
(terpri)

(print "Testing set_board with case 4")
(terpri)
(terpri)

(
  print_board
      (
        cartesian_board
              (
                get_board
                      (
                        set_board
                        (case_4)
                        (
                          set_stone
                            (
                              get_board (case_4)
                            )

                            "A1"
                            'P
                        )
                      )
              )
      )
)

(terpri)
(terpri)

(print "Testing get_current_player with case 3 and 4")

(format 
      t
      "Current player for case 4: ~a~%"
      (
        get_current_player 

        (case_4)
      )
)
(format 
      t
      "Current player for case 3: ~a~%"
      (
        get_current_player 

        (case_3)
      )
)


(terpri)
(terpri)

(print "Testing swtich_stone with case 3 and 4")


(print "Case 3")
(print (case_3))
(print (switch_turn (case_3)))

(terpri)

(print "Case 4")
(print (case_4))
(print (switch_turn (case_4)))

(terpri)
(terpri)

(print "Testing get_distance -- 0 distance")
(terpri)
(
  format 
        t
        "Distance between A1 and A1: ~a~%"
        (
          get_distance
            "A1"
            "A1"
        )
)

(print "Testing get_distance")
(terpri)
(
  format 
        t
        "Distance between A1 and A2: ~a~%"
        (
          get_distance
            "A1"
            "A2"
        )
)
(print "Testing get_distance")
(terpri)
(
  format 
        t
        "Distance between a1 and b2: ~a~%"
        (
          get_distance
            "A1"
            "b2"
        )
)
(print "Testing get_distance")
(terpri)
(
  format 
        t
        "Distance between j10 and m8: ~a~%"
        (
          get_distance
            "j10"
            "M8"
        )
)
(print "Testing get_distance")
(terpri)
(
  format 
        t
        "Distance between A1 and S19: ~a~%"
        (
          get_distance
            "A1"
            "S19"
        )
)
(print "Testing get_distance")
(terpri)
(
  format 
        t
        "Distance between A1 and d3: ~a~%"
        (
          get_distance
            "A1"
            "d3"
        )
)

(terpri)
(terpri)

(print "Testing get_empty_board with 19 rows and 19 columns")
(terpri)

(
  print_board
        (
          cartesian_board
            (
              get_empty_board
                19
                19
            )
        )
)

(terpri)
(terpri)

(print "Testing get_initial_state")
(terpri)

(
  print (get_initial_state)
) 

(terpri)
(terpri)

(print "Testing get_center")
(terpri)

(
  print (get_center (get_empty_board 19 19))
)

(terpri)
(terpri)

(print "Testing get_available_moves with the initial state")
(terpri)

(
  print (get_available_moves
                   (get_board (get_initial_state))
        ) 
)


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



