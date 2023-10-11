(load "file_handling.lisp")
(load "game_state.lisp")


;;;; ******************************************************************
;;;; Human Player functions
;;;; Used for getting inputs from the human
;;;; ******************************************************************



;;; *********************************************
;;; Name   : get_human_move
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;;          file_name is the name of the file to
;;;          save the game to
;;; Purpose: Get the human's move for a game
;;;          state
;;; Return : The position -- a string like "A1"
;;; *********************************************
(defun get_human_move (game_state)


    (princ "Enter your move: ")
    (terpri)
    (let*
        ( 
            (move (read-line))
        )

        (cond 
                    (
                        ; if user wants to quit, ie. "q"
                        (equal (string-upcase move) "Q")
                        (save_and_quit game_state)
                    )

                    (
                        ; if the move is a valid move, ie. in the list of available moves,
                        ; then return the move
                        (member 
                                ; convert the move to uppercase
                                (string-upcase move) 
                                (get_available_moves (get_board game_state))

                                :test #'string=
                        )
                        
                        (string-upcase move)
                    )

                    (
                        ; if the move is not valid, then print an error message and
                        ; get the move again
                        t
                        (print "Invalid move. Please try again.")
                        (terpri)
                        (get_human_move game_state)
                    )
        )
    
    )

)

;;; *********************************************
;;; Name   : save_and_quit
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;;          file_name is the name of the file to
;;;          save the game to
;;; Purpose: Save the game and quit
;;; Return : None
;;; *********************************************
(defun save_and_quit (game_state)
    (save_game_to_user_location game_state)
    (quit)
)

;;; *********************************************
;;; Name   : human_wins_toss
;;; Args   : None
;;; Purpose: Toss a coin to see who goes first
;;; Return : t if human wins, nil otherwise
;;; *********************************************
(defun human_wins_toss ()
    
    (princ "Tossing a coin to see who goes first...")
    (terpri)
    (princ "Heads or tails? (h/t): ")
    (terpri)

    (let*
        (
            (human_choice (read-line))
            (coin_toss (random 2))
        )

        (cond 

            ; if the inputs are invalid, then try again
            (
                (not (member (string-upcase human_choice) '("H" "T") :test #'string=))
                (princ "Invalid input. Please try again.")
                (terpri)
                (human_wins_toss)
            )

            (
                (equal coin_toss 1)
                (princ "You won the toss!")
                (terpri)
                t
            )

            (
                (equal coin_toss 0)
                (princ "You lost the toss!")
                (terpri)
                nil
            )
        )
    )
)



;;;; ******************************************************************
;;;; End of Human Player functions
;;;; ******************************************************************