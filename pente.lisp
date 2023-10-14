(load "board.lisp")
(load "game_state.lisp")
(load "strategy.lisp")
(load "file_handling.lisp")
(load "serials.lisp")
(load "human.lisp")
(load "computer.lisp")
(load "round.lisp")
(load "tournament.lisp")

; Seed the random number generator
; Otherwise, the random number generator will always
; generate the same sequence of numbers
; This is the only place where setf is used
(setf *random-state* (make-random-state t))

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