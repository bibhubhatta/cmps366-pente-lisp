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


;;; *********************************************
;;; Name   : get_player_from_stone
;;; Args   : game_state, stone
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Get the player that is playing the stone
;;; Return : The player -- Human or Computer or nil if the stone
;;;         is invalid
;;; *********************************************
(defun get_player_from_stone (game_state stone)
    
    (cond
        ; if the stone is provided as a full word
        ((equal stone (nth 6 game_state)) (nth 5 game_state))
        ((equal (other_stone stone) (nth 6 game_state)) (other_player (nth 5 game_state)))

        ; if the stone is provided as a single character
        ((equal stone 'W) (get_player_from_stone game_state 'White))
        ((equal stone 'B) (get_player_from_stone game_state 'Black))
    )
)

;;; *********************************************
;;; Name   : get_tournament_score
;;; Args   : game_state, player
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Get the score of the player
;;; Return : The score -- a number
;;; *********************************************
(defun get_tournament_score (game_state player)
  (cond
    ((equal player 'Human) (nth 2 game_state))
    ((equal player 'Computer) (nth 4 game_state))
  )
)

;;; *********************************************
;;; Name   : is_stone
;;; Args   : stone
;;;          stone is the stone to check
;;; Purpose: Check if the argument is a stone
;;; Return : t if the argument is a stone, nil otherwise
;;; *********************************************
(defun is_stone (stone)
  (or (equal stone 'W) (equal stone 'B) (equal stone 'White) (equal stone 'Black))
)

;;; *********************************************
;;; Name   : get_no_captures
;;; Args   : game_state, player
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Get the number of captures of the player
;;; Return : The number of captures -- a number
;;; *********************************************

(defun get_no_captures (game_state player)
  (cond
    ((equal player 'Human) (nth 1 game_state))
    ((equal player 'Computer) (nth 3 game_state))

    ; if the stone is supplied instead of the player
    ((is_stone player) (get_no_captures game_state (get_player_from_stone game_state player)))
  )
)

;;; *********************************************
;;; Name   : set_no_captures
;;; Args   : game_state, player, no_captures
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Set the number of captures of the player
;;; Return : The new game state
;;; *********************************************
(defun set_no_captures (game_state player no_captures)
  (cond
    ((equal player 'Human) (replace_nth 1 no_captures game_state))
    ((equal player 'Computer) (replace_nth 3 no_captures game_state))

    ; if the stone is supplied instead of the player
    ((is_stone player) (set_no_captures game_state (get_player_from_stone game_state player) no_captures))
  )
)

;;; *********************************************
;;; Name   : set_board
;;; Args   : game_state, board
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Set the board of the game state
;;;          to the board supplied
;;;          This will be used to updated the board
;;;          after a move is made
;;; Return : The new game state
;;; *********************************************
(defun set_board (game_state board)
  (cons board (cdr game_state))  
)

;;; *********************************************
;;; Name   : get_current_player
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Get the current player
;;; Return : The current player -- Human or Computer
;;; *********************************************
(defun get_current_player (game_state)
  (nth 5 game_state)
)


;;; *********************************************
;;; Name   : get_current_stone
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Get the current stone
;;; Return : The current stone -- W or B
;;; *********************************************
(defun get_current_stone (game_state)
  (nth 6 game_state)
)

;;; *********************************************
;;; Name   : replace_nth
;;; Args   : n, new, list
;;;          n is the index of the element to be replaced
;;;          new is the new element
;;;          list is the list
;;; Purpose: Replace the nth element of the list with the new element
;;; Return : The new list
;;; *********************************************
(defun replace_nth (n new list)
  (cond
    ((equal n 0) (cons new (cdr list)))
    (t (cons (car list) (replace_nth (- n 1) new (cdr list))))
  )
)

;;; *********************************************
;;; Name   : set_current_player
;;; Args   : game_state, player
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Set the current player
;;; Return : The new game state
;;; *********************************************
(defun set_current_player (game_state player)
  (replace_nth 5 player game_state)
)

;;; *********************************************
;;; Name   : switch_player
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Switch the player of the game state
;;; Return : The new game state
;;; *********************************************
(defun switch_player (game_state)

  (replace_nth
                 5 ; index of the current player
                 (other_player (nth 5 game_state))
                 game_state
  )

)

;;; *********************************************
;;; Name   : switch_stone
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Switch the stone of the game state
;;; Return : The new game state
;;; *********************************************
(defun switch_stone (game_state)

  (replace_nth
                 6 ; index of the current stone
                 (other_stone (nth 6 game_state))
                 game_state
  )

)


;;; *********************************************
;;; Name   : switch_turn
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Switch the turn of the game state
;;; Return : The new game state
;;; *********************************************
(defun switch_turn (game_state)

  (
    switch_player
          (
            switch_stone game_state
          )
  )

)


;;; *********************************************
;;; Name   : get_initial_state
;;; Purpose: Get the initial game state
;;; Return : The initial game state
;;; *********************************************
(defun get_initial_state ()

  (list

    ; empty board
    (get_empty_board 19 19)

    ; no captures for human
    0

    ; human score
    0

    ; no captures for computer
    0

    ; computer score
    0

    ; current player is nil because it is yet
    ; to be decided
    nil

    ; current stone is white because white
    ; always starts first
    'White


  )

)

(defun make_move (game_state move)

  (cond 
  
      (
        ; if the move is valid
        (member move (get_available_moves (get_board game_state)) :test #'string=)

        (
          ; switch the turn
          switch_turn
            ( 
              
                ; update the game state with the new board
                set_board game_state
                    (
                      ; set the stone on the board
                      set_stone (get_board game_state) move (get_current_stone game_state)
                    )      
              
            )
        
        )
      
      
      )

      (
        ; if the move is invalid
        t
        (format t "Invalid move!~%")
        ;return the same game state
        game_state
      )

  )

)

;;; *********************************************
;;; Name   : get_winner
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Get the winner of the game
;;; Return : The winner -- Human or Computer or nil if the game
;;;          is not over
;;; *********************************************
(defun get_winner (game_state)
  
  (cond
    ; check captures

    (
      (
        >= 
        (get_no_captures game_state 'Human)
        5
      )
      'Human
    )

    (
      (
        >= 
        (get_no_captures game_state 'Computer)
        5
      )
      'Computer
    )

    ; check if there is a sequence equal or longer
    ; than 5

    (
      ( >=
        ( 
          length
                (
                  remove-if-not
                    #'(lambda (sequence)
                        (>= (length sequence) 5)
                      )
                      
                      (get_all_stone_sequences 
                            (get_board game_state)
                            (get_stone_from_player game_state 'Human)
                      )
                )
        )
        1
      )
      'Human
    )

    (
      ( >=
        ( 
          length
                (
                  remove-if-not
                    #'(lambda (sequence)
                        (>= (length sequence) 5)
                      )

                      (get_all_stone_sequences 
                            (get_board game_state)
                            (get_stone_from_player game_state 'Human)
                      )
                )
        )
        1
      )
      'Computer
    )
    
    (t nil)
  )


)

;;; *********************************************
;;; Name   : is_game_drawn
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Check if the game is drawn
;;; Return : t if the game is drawn, nil otherwise
;;; *********************************************

(defun is_game_drawn (game_state)
  (cond
    (
      ; if there are no available moves
      (null (get_available_moves (get_board game_state)))
      t
    )

    (t nil)

  )
)

;;; *********************************************
;;; Name   : is_game_over
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Check if the game is over
;;; Return : t if the game is over, nil otherwise
;;; *********************************************

(defun is_game_over (game_state)
  
    (or
      (get_winner game_state)
      (is_game_drawn game_state)
    )
)

;;; *********************************************
;;; Name   : get_round_score
;;; Args   : game_state, player
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Get the score of the player
;;; Return : The score -- a number
;;; *********************************************
(
  defun get_round_score (game_state player)
    (
      +
      (
        get_sequence_score (get_board game_state) (get_stone_from_player game_state player)
      )
      (
        get_no_captures game_state player
      )
    )
)


;;; *********************************************
;;; Name   : print_game_state
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Display the game state
;;; Return : nil
;;; *********************************************
(defun print_game_state (game_state)

  (format t "~%")
  (format t "Board:~%")
  (print_board (cartesian_board (get_board game_state)))
  (format t "~%")
  (format t "Human captures: ~a~%" (get_no_captures game_state 'Human))
  (format t "Computer captures: ~a~%" (get_no_captures game_state 'Computer))
  (format t "Human score: ~a~%" (get_round_score game_state 'Human))
  (format t "Computer score: ~a~%" (get_round_score game_state 'Computer))
  (format t "Current player: ~a~%" (get_current_player game_state))
  (format t "Current stone: ~a~%" (get_current_stone game_state))
  (format t "~%")

)

;;; *********************************************
;;; Name   : get_tournament_score
;;; Args   : game_state, player
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Get the score of the player
;;; Return : The score -- a number
;;; *********************************************
(defun get_tournament_score (game_state player)
  (cond
    ((equal player 'Human) (nth 2 game_state))
    ((equal player 'Computer) (nth 4 game_state))
  )
)

;;; *********************************************
;;; Name   : set_tournament_score
;;; Args   : game_state, player, score
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Set the score of the player
;;; Return : The new game state
;;; *********************************************
(defun set_tournament_score (game_state player score)
  (cond
    ((equal player 'Human) (replace_nth 2 score game_state))
    ((equal player 'Computer) (replace_nth 4 score game_state))
  )
)

;;; *********************************************
;;; Name   : update_tournament_score
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Update the tournament score of both
;;;          players if the game is over
;;; Return : The new game state
;;; *********************************************
(defun update_tournament_score (game_state)

  (cond
        (
          ; if the game is over, update the score
          ; of both players
          (get_winner game_state)
          
          (
            ; update the computer score
            set_tournament_score
              (
                ;update the human score
                set_tournament_score
                  game_state
                  'Human
                  (
                    +
                    (get_tournament_score game_state 'Human)
                    (get_round_score game_state 'Human)
                  )
              )
              'Computer
              (
                +
                (get_tournament_score game_state 'Computer)
                (get_round_score game_state 'Computer)
              )
          )
        )

  )

)

;;; *********************************************
;;; Name   : initialize_round
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Initialize the game state for a new round
;;; Return : The new game state
;;; *********************************************
(defun initialize_round (game_state)

  
 (set_current_player
    
    ; set new board
    (set_board

      ; set both captures to 0
      (
        set_no_captures
          (
            set_no_captures
              game_state
              'Human
              0
          )
          'Computer
          0
      )

      (get_empty_board 19 19)
    )
    
    nil
  
)

)

;;;; ******************************************************************
;;;; End of game state related functions
;;;; ******************************************************************




