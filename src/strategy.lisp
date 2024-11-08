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
;;; Name   : win_blocking_move
;;; Arg    : game_state, move
;;; Purpose: To check if the given move prevents
;;;          the opponent from winning by playing
;;;          that move
;;; Return : T if it is a win blocking move, nil otherwise
;;; Algo   : Switches the turn and checks if the
;;;          move is a winning move
;;; *********************************************
(defun win_blocking_move (game_state move)

    (cond 
            ; if there is a winner after move is made, then
            ; return t
            (
                (get_winner (make_move (switch_turn game_state) move))
                t
                
            )
            
            (t nil)
        
    )

)

;;; *********************************************
;;; Name   : capturing_move
;;; Arg    : game_state, move
;;; Purpose: To check if the given move is a
;;;          capturing move
;;; Return : t if it is a capturing move, nil otherwise
;;; Algo   : Makes the move and checks if the
;;;          no. captured stones changes
;;; *********************************************
(defun capturing_move (game_state move)

    ; Use let to optimize the code
    (let*
    
        (
            (current_player (get_current_player game_state))
            (opponent (other_player current_player))
            (game_state_after_move (make_move game_state move))
        )
        
        (cond 
                ; if the no. of captured stones hasnt changed, then
                ; return nil
                (
                    (= 
                        (get_no_captures game_state current_player)
                        (get_no_captures game_state_after_move current_player)
                    )
                    nil
                    
                )
                
                (t t)
            
        )
    )

)


;;; *********************************************
;;; Name   : capture_blocking_move
;;; Arg    : game_state, move
;;; Purpose: To check if the given move is a
;;;          capture blocking move
;;; Return : t if it is a capture blocking move, nil otherwise
;;; Algo   : Switches the turn and checks if the
;;;          move is a capturing move
;;; *********************************************
(defun capture_blocking_move (game_state move)

   (capturing_move (switch_turn game_state) move)

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

    ; Make the move first and then check
    ; if there is an opponent's stone in its
    ; neighborhood, because it might be a capturing
    ; move

    (let
    
        (
            (game_state_after_move (make_move game_state move))
            (current_stone (get_current_stone game_state))
        )
        
        (cond

                (
                    (stone_in_list 
                            (other_stone current_stone)  
                            (get_neighboring_stones (get_board game_state_after_move) move) 
                    )
                    t
                )

                (t nil)
        )
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
;;; Name   : get_pseudo_sequence_score_localized
;;; Arg    : game_state, player, position
;;; Purpose: Calculates the pseudo score for the
;;;          given player's sequences
;;; Return : The pseudo score
;;; Algo   : The pseudo score is calculated by
;;;          summing the square of the length of
;;;          localized sequences that are longer than 1
;;; *********************************************
(defun get_pseudo_sequence_score_localized (game_state player position)

    (let*
    
        (
            (sequences (get_all_stone_sequences_localized (get_board game_state) position))
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
            (opponent (other_player (get_current_player game_state)))
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
;;; Name   : get_pseudo_score_localized
;;; Arg    : game_state, move
;;; Purpose: Calculates the pseudo score for the
;;;          given move
;;; Return : The pseudo score
;;; Algo   : The pseudo score is calculated by
;;;          adding the score for the player
;;;          and the opponent if the move is played
;;;          by both
;;; *********************************************
(defun get_pseudo_score_localized (game_state move)
    ; (declare (optimize (speed 3) (safety 0)))
    ; (declare (type list game_state))
    ; (declare (type string move))

    (let*
    
        (
            (current_player (get_current_player game_state))
            (opponent (other_player (get_current_player game_state)))
            (game_state_after_move (make_move game_state move))
            
            ; check what happens if opponent plays that move
            (game_state_if_opponent_move (make_move 
                                         (switch_turn game_state) 
                                         move
                                         )
            )
        
        )
        
        (+ 
            (* (get_round_score_localized game_state_after_move current_player move) 1000)
            (* (get_round_score_localized game_state_if_opponent_move opponent move) 1000)

            ; Prioritze capturing moves over 4 in a sequence
            (* (get_no_captures game_state_after_move current_player) 1000)
            ; prioritize offensive capture over defensive blocking
            ; by giving less weightage to the opponent's captures
            (* (get_no_captures game_state_if_opponent_move opponent) 500)
            (* (get_pseudo_sequence_score_localized game_state_after_move current_player move) 10)
            (* (get_pseudo_sequence_score_localized game_state_if_opponent_move opponent move) 10)
            ; subtract the distance from the center to prevent playing at the edges
            (- 0 (get_distance (get_center (get_board game_state)) move))
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
;;; Name   : get_pseudo_scores_localized
;;; Arg    : game_state, moves
;;; Purpose: Calculates the pseudo scores for the
;;;          given moves
;;; Return : A list of the list of moves and their
;;;          pseudo scores
;;; *********************************************
(defun get_pseudo_scores_localized (game_state moves)

    (mapcar (lambda (move) (list move (get_pseudo_score_localized game_state move))) moves)

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
    
    (let
            (
                (available_moves_with_neighbors (get_available_positions_with_neighbors (get_board game_state)))
            )

            (cond 
                    ; if it's the third move, return a random position 3 intersections
                    ; away from the center
                    (
                        (is_third_move game_state)

                        (let
                                (
                                    (3_away (get_positions_3_away_from_center (get_board game_state)))
                                )

                                (nth (random (length 3_away)) 3_away)
                        )
                    )

                    (
                        (not (null available_moves_with_neighbors))
                        (caar
                           
                                (
                                    sort
                                    (get_pseudo_scores_localized game_state available_moves_with_neighbors)
                                    (lambda (x y) (> (second x) (second y)))
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

    (caar
        
            (
                sort
                (get_pseudo_scores game_state (get_available_moves (get_board game_state)))
                (lambda (x y) (> (second x) (second y)))
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
        let
    
            (
                (available_moves (get_available_moves (get_board game_state)))
            )
            
            (nth (random (length available_moves)) available_moves)
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
                                               'win_blocking_move
                                               'capturing_move
                                               'capture_blocking_move
                                               'sequence_making_move
                                               'sequence_blocking_move
                                         )
                )
            )

            ; return the names of the functions that return t
            (remove-if-not (lambda (function) (funcall function game_state move)) move_analysis_functions)
    )

)

;;; *********************************************
;;; Name   : get_rationale_explanation
;;; Arg    : rationale
;;; Purpose: To get the explanation for the given rationale
;;; Return : A sentence string explaining the rationale
;;; *********************************************

(defun get_rationale_explanation (rationale)

    (cond

            (
                (eq rationale 'only_move)
                "is the only available move."
            )

            (
                (eq rationale 'winning_move)
                "is a winning move."
            )

            (
                (eq rationale 'win_blocking_move)
                "prevents the opponent from winning."
            )

            (
                (eq rationale 'capturing_move)
                "is a capturing move."
            )

            (
                (eq rationale 'capture_blocking_move)
                "prevents the opponent from capturing."
            )

            (
                (eq rationale 'sequence_making_move)
                "is a sequence making move."
            )

            (
                (eq rationale 'sequence_blocking_move)
                "prevents the opponent from making a sequence."
            )

            (t "is a random move closest to the center.")

    )
)

;;; *********************************************
;;; Name   : get_explanation_from_rationales
;;; Arg    : rationales
;;;          A list of rationales
;;; Purpose: To get the rationale explanations for the given move
;;; Return : One or multiple sentence strings explaining the rationale
;;;         It is one string only
;;; *********************************************
(defun get_explanation_from_rationales (rationales)

    (cond

            (
                ; if there are no rationales, then it is a random move
                ; closest to the center for the third move
                ; because all other moves are around other stones
                (null rationales)
                "The move is a random move closest to the center that is 3 intersections away."
            )
            
            (
                ; if there is only one rationale, then
                ; return the explanation
                (eq (length rationales) 1)
                (concatenate 'string
                             "The move "
                             (get_rationale_explanation (car rationales))
                )
            )

            
            ; if there are multiple rationales, then
            ; return the explanations concatenated
            (t
                (concatenate 'string
                            
                            "The move "
                            (get_rationale_explanation (car rationales))
                            " "
                        
                            ; concatenate the explanations with "it also"

                            (format 
                                    nil
                                    
                                    "~{It also ~a~^ ~}" 
                                    
                                    ; get the explanations for each rationale
                                    (mapcar #'get_rationale_explanation (cdr rationales))
                            )
                )
            )
    )

)

;;; ******************************************************************
;;; End of strategy related functions
;;; ******************************************************************
