(load "game_state.lisp")


;;;; ******************************************************************
;;;; Strategy related functions
;;;; ******************************************************************


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

; (trace get_pseudo_score)
; (trace get_pseudo_scores)