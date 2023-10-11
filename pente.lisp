(load "tournament.lisp")


;;;; ******************************************************************
;;;; Runner for the Pente game
;;;; ******************************************************************


;;; *********************************************
;;; Name   : play_pente
;;; Arg    : None
;;; Purpose: Runs the game; if user wants to load
;;;          a game, it loads the game and plays
;;;          the tournament; otherwise, it plays
;;;          the tournament with a new game state
;;; Return : The final game state after the tournament
;;;          is over
;;; *********************************************
(defun play_pente ()

    (cond 
            (
                (human_wants_to_load_game)

                (
                    play_tournament (load_game_state_from_user_location)
                )
            )

            (t

                (
                    play_tournament (get_initial_state)
                )


            )

    )
)

;;;; ******************************************************************
;;;; End of runner for the Pente game
;;;; ******************************************************************


; Run game
(play_pente)