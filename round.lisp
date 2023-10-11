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

