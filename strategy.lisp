(load "game_state.lisp")
(load "board.lisp")


;;;; ******************************************************************
;;;; Strategy related functions
;;;; ******************************************************************


;;; *********************************************
;;; Name   : winning_move
;;; Arg    : game_state, move
;;; Purpose: To check if the given move is a
;;;          winning move
;;; Return : T if it is a winning move, nil otherwise
;;; Algo   : It checks if the move leads to a win
;;; *********************************************
(defun winning_move (game_state move)

    (cond 
            ; if there is a winner after move is made, then
            ; return t
            (
                (get_winner (make_move game_state move))
                t
                
            )
            
            (t nil)
        
    )

)

;;; *********************************************
;;; Name   : only_move
;;; Arg    : game_state, move
;;; Purpose: To check if the given move is the
;;;          only move available
;;; Return : t if it is the only move, nil otherwise
;;; Algo   : It checks if the move is the only move
;;;          available
;;; *********************************************
(defun only_move (game_state move)

    (cond 
            ; if there is only one move available, then
            ; return t
            (
                (= (length (get_available_moves (get_board game_state))) 1)
                t   
            )
            (t nil)
        
    )
)

;;; *********************************************
;;; Name   : stone_in_list
;;; Arg    : stone, list
;;; Purpose: To check if the given stone is in the
;;;          given list. This was necessary because
;;;          member could not be used with both full
;;;          words and single letter representations
;;; Return : t if it is in the list, nil otherwise
;;; *********************************************
(defun stone_in_list (stone list)

    (cond 

            ; if the stone is in the full word representation
            ; call the function again with the single letter
            (
                (equal stone 'White)
                (stone_in_list 'W list)
            )

            (
                (equal stone 'Black)
                (stone_in_list 'B list)
            )

            (
                (equal stone 'Empty)
                (stone_in_list 'O list)
            )

        
            ; if the stone is in the list, then
            ; return t
            (
                (member stone list :test #'equal)
                t   
            )
            (t nil)
        
    )
)

;;; *********************************************
;;; Name   : sequence_making_move
;;; Arg    : game_state, move
;;; Purpose: To check if the given move is a
;;;          sequence making move
;;; Return : t if it is a sequence making move, nil otherwise
;;; Algo   : Checks if there is a same stone in its
;;;          neighborhood
;;; *********************************************
(defun sequence_making_move (game_state move)

    (cond

            (
                (stone_in_list 
                        (get_current_stone game_state)
                        (get_neighboring_stones (get_board game_state) move) 
                )
                t
            )

            (t nil)
    )
)



;;; *********************************************
;;; Name   : sequence_blocking_move
;;; Arg    : game_state, move
;;; Purpose: To check if the given move is a
;;;          sequence blocking move
;;; Return : t if it is a sequence blocking move, nil otherwise
;;; Algo   : Checks if there is a opponent's stone in its
;;;          neighborhood
;;; *********************************************
(defun sequence_blocking_move (game_state move)

    (cond

            (   
                (stone_in_list
                        (other_stone (get_current_stone game_state))  
                        (get_neighboring_stones (get_board game_state) move) 
                )
                t
            )
            (t nil)
    )
)


;;; *********************************************
;;; Name   : get_pseudo_sequence_score
;;; Arg    : game_state, player
;;; Purpose: Calculates the pseudo score for the
;;;          given player's sequences
;;; Return : The pseudo score
;;; Algo   : The pseudo score is calculated by
;;;          summing the square of the length of
;;;          sequences that are longer than 1
;;; *********************************************
(defun get_pseudo_sequence_score (game_state player)

    (let*
    
        (
            (sequences (get_all_stone_sequences (get_board game_state) (get_stone_from_player game_state player)))
            (long_sequences (remove-if (lambda (sequence) (< (length sequence) 2)) sequences))
        )
        
        (reduce #'+ (mapcar (lambda (sequence) (* (length sequence) (length sequence))) long_sequences))

    )
)


;;; *********************************************
;;; Name   : get_pseudo_score
;;; Arg    : game_state, move
;;; Purpose: Calculates the pseudo score for the
;;;          given move
;;; Return : The pseudo score
;;; Algo   : The pseudo score is calculated by
;;;          adding the score for the player
;;;          and the opponent if the move is played
;;;          by both
;;; *********************************************
(defun get_pseudo_score (game_state move)
    ; (declare (optimize (speed 3) (safety 0)))
    ; (declare (type list game_state))
    ; (declare (type string move))

    (let*
    
        (
            (current_player (get_current_player game_state))
            (opponent (other_player current_player))
            (game_state_after_move (make_move game_state move))
            
            ; check what happens if opponent plays that move
            (game_state_if_opponent_move (make_move 
                                         (switch_turn game_state) 
                                         move
                                         )
            )
        
        )
        
        (+ 
            (* (get_round_score game_state_after_move current_player) 1000)
            (* (get_round_score game_state_if_opponent_move opponent) 1000)
            (* (get_pseudo_sequence_score game_state_after_move current_player) 10)
            (* (get_pseudo_sequence_score game_state_if_opponent_move opponent) 10)
        )

    )
)

;;; *********************************************
;;; Name   : get_pseudo_scores
;;; Arg    : game_state, moves
;;; Purpose: Calculates the pseudo scores for the
;;;          given moves
;;; Return : A list of the list of moves and their
;;;          pseudo scores
;;; *********************************************
(defun get_pseudo_scores (game_state moves)

    (mapcar (lambda (move) (list move (get_pseudo_score game_state move))) moves)

)


;;; *********************************************
;;; Name   : get_best_move_optimized
;;; Arg    : game_state - the current game state
;;; Purpose: To get the best move for the game state
;;; Return : The best move -- a position string
;;; Algo   : The best move is the one with the highest
;;;          pseudo score. The pseudo score is calculated
;;;          for available moves, and sorted to find the
;;;          one with the highest score. The available 
;;;          moves are filtered to check if they have
;;;          any neighbors.
;;; *********************************************
(defun get_best_move_optimized (game_state)
    
    (let*
            (
                (available_moves (get_available_moves (get_board game_state)))
                (available_moves_with_neighbors (get_available_positions_with_neighbors (get_board game_state)))
            )

            (cond 

                    ; if there is only one move available, return it
                    ((= (length available_moves) 1) (car available_moves))

                    ; if it's the third move, return random move for now
                    ((is_third_move (get_board game_state)) (get_random_move game_state))

                    (
                        (not (null available_moves_with_neighbors))
                        (car
                            (car
                                (
                                    sort
                                    (get_pseudo_scores game_state available_moves_with_neighbors)
                                    (lambda (x y) (> (second x) (second y)))
                                )
                            )
                        )
                    )

                    (t (get_random_move game_state))
            )
    
    )

)


;;; *********************************************
;;; Name   : get_best_move
;;; Arg    : game_state - the current game state
;;; Purpose: To get the best move for the game state
;;; Return : The best move -- a position string
;;; Algo   : The best move is the one with the highest
;;;          pseudo score. The pseudo score is calculated
;;;          for available moves, and sorted to find the
;;;          one with the highest score.
;;; *********************************************
(defun get_best_move (game_state)

    (car
        (car
            (
                sort
                (get_pseudo_scores game_state (get_available_moves (get_board game_state)))
                (lambda (x y) (> (second x) (second y)))
            )
        )
    )

)

;;; *********************************************
;;; Name   : get_random_move
;;; Arg    : game_state - the current game state
;;; Purpose: To get a random move for the game state
;;; Return : A random move -- a position string
;;; Algo   : A random move is chosen from the list
;;;          of available moves
;;; *********************************************
(defun get_random_move (game_state)

    (
        nth
        (random (length (get_available_moves (get_board game_state))))
        (get_available_moves (get_board game_state))
    )

)


;;; *********************************************
;;; Name   : get_move_rationale
;;; Arg    : game_state, move
;;; Purpose: To get the rationale for the given move
;;; Return : A list of reasons
;;; Algo   : It checks if the move leads to various
;;;          outcomes and returns the rationale
;;; *********************************************
(defun get_move_rationale (game_state move)
    (let*

            (
                (move_analysis_functions (list 'only_move
                                               'winning_move
                                                'sequence_making_move
                                                'sequence_blocking_move
                                         )
                )
            )

            ; return the names of the functions that return t
            (remove-if-not (lambda (function) (funcall function game_state move)) move_analysis_functions)
    )

)

;;; ******************************************************************
;;; End of strategy related functions
;;; ******************************************************************
