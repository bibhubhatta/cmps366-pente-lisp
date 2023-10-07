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


;;;; ******************************************************************
;;;; End of game state related functions
;;;; ******************************************************************




