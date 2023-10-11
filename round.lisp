(load "board.lisp")
(load "game_state.lisp")
(load "serials.lisp")
(load "human.lisp")
(load "computer.lisp")


;;;; ******************************************************************
;;;; Round related functions
;;;; ******************************************************************


;;; *********************************************
;;; Name   : play_round
;;; Arg    : game_state - the current game state
;;; Purpose: Continues the game until it is over
;;; Return : The final game state after the game is over
;;; *********************************************
(defun play_round (game_state)

    (let*

        (
            ; set the starting player if the game is just starting
            ; the let* was unavoidable because the state has to be passed
            ; to get_move which returns the move and not the game state
            ; if it passed to get_move, the change won't persist
            ; for make_move

            (game_state (set_starting_player game_state))
        )


        (cond 
            ; if the game is over, return the game state
            ((is_game_over game_state) game_state)
            
             (t

                (print_game_state game_state)

                (play_round 
                     (make_move
                            game_state
                            ; get current player's move
                            (get_move game_state)
                    )
                )
             )
        )

    
    
    )
    

)

;;; *********************************************
;;; Name   : get_move
;;; Arg    : game_state - the current game state
;;; Purpose: Gets the move from the current player
;;; Return : The move from the current player
;;; *********************************************
(defun get_move (game_state)
    (cond 
            (
                (equal (get_current_player game_state) 'human)
                (get_human_move game_state)
            )

            (
                (equal (get_current_player game_state) 'computer)
                (get_computer_move game_state)
            )
    )
)

;;; *********************************************
;;; Name   : set_starting_player
;;; Arg    : game_state - the current game state
;;; Purpose: Sets the starting player if the 
;;;          game is just starting
;;; Return : The game state with the starting player set
;;; *********************************************
(defun set_starting_player (game_state)
   
    (cond 
        (
            ; if the round is not initialized, i.e. the
            ; starting player has already been set
            ; return the game state
            (get_current_player game_state)
            game_state
        )

        (
            ; if the tournament score of the human is greater than
            ; the score of the computer, set the starting
            ; player to the human
            (>
                    (get_tournament_score game_state 'human)
                    (get_tournament_score game_state 'computer)
            )

            (set_current_player game_state 'human)
        )

        (
            ; if the tournament score of the computer is greater than
            ; the score of the human, set the starting
            ; player to the computer
            (>
                    (get_tournament_score game_state 'computer)
                    (get_tournament_score game_state 'human)
            )

            (set_current_player game_state 'computer)
        )

        (
            ; if human wins the coin toss, set the starting player
            ; to human
            (human_wins_toss) (set_current_player game_state 'human)
        )

        (
            ; otherwise, set the starting player to computer
            t (set_current_player game_state 'computer)
        )
    )

)


