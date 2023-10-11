(load "board.lisp")
(load "game_state.lisp")
(load "serials.lisp")
(load "human.lisp")
(load "computer.lisp")
(load "round.lisp")


;;;; ******************************************************************
;;;; Tournament related functions
;;;; ******************************************************************


;;; *********************************************
;;; Name   : play_tournament
;;; Arg    : game_state
;;; Purpose: Continues playing the tournament
;;;           until the human player quits
;;; Return : The final game state after the tournament
;;;          is over
;;; *********************************************
(defun play_tournament (game_state)

    ; Announce the tournament scores
    (announce_tournament_scores game_state)
    (terpri)


    (let*
        
        (
            (final_game_state (
                                update_tournament_score (conduct_round game_state)
                          )
        ))

        ; Announce the tournament scores
        (announce_tournament_scores final_game_state)

        
        (cond
                (
                    ; If the human wants to play again, play another round
                    ; after initializing the round
                    (human_wants_to_play_again)
                    (play_tournament 
                        
                       (initialize_round final_game_state)
            
                    )
                )

                (
                    ; Otherwise, announce the results and return the game state
                    (announce_tournament_result final_game_state)
                    game_state
                )

        )
    )
)


;;; *********************************************
;;; Name   : announce_tournament_scores
;;; Arg    : game_state
;;; Purpose: Announces the tournament scores
;;; Return : None
;;; *********************************************
(defun announce_tournament_scores (game_state)
    (format t "~%Tournament Scores~%")
    (format t "------------------~%")
    (format t "Human: ~a~%" (get_tournament_score game_state 'Human))
    (format t "Computer: ~a~%" (get_tournament_score game_state 'Computer))
)


;;; *********************************************
;;; Name   : announce_tournament_result
;;; Arg    : game_state
;;; Purpose: Announces the tournament result
;;; Return : None
;;; *********************************************
(defun announce_tournament_result (game_state)

    (format t "~%~%----------------------------------~%")
    (format t "Tournament Result~%")
    (format t "----------------------------------")
    (announce_tournament_scores game_state)

    (cond
            
            (
                ; if the tournament has a winner
                (get_tournament_winner game_state)
                (format t "~%The tournament winner is ~a!~%"
                    (get_tournament_winner game_state)
                )
                (format t "----------------------------------~%")
            )
            
            
            (
                ; if the tournament is a tie
                t
                (format t "The tournament is a tie!~%")
                (format t "----------------------------------~%")
            )


    )

)


;;; *********************************************
;;; Name   : get_tournament_winner
;;; Arg    : game_state
;;; Purpose: Returns the winner of the tournament
;;; Return : The winner of the tournament
;;; *********************************************
(defun get_tournament_winner (game_state)

    (cond
            (
                ; if the tournament is a tie
                (equal
                    (get_tournament_score game_state 'Human)
                    (get_tournament_score game_state 'Computer)
                )

                nil
            )

            (
                ; if the human won the tournament
                (>
                    (get_tournament_score game_state 'Human)
                    (get_tournament_score game_state 'Computer)
                )

                'Human
            )

            (
                ; if the computer won the tournament
                (>
                    (get_tournament_score game_state 'Computer)
                    (get_tournament_score game_state 'Human)
                )

                'Computer
            )

    )

)

;;;; ******************************************************************
;;;; End of tournament related functions
;;;; ******************************************************************
