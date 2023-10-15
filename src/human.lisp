;;;; ******************************************************************
;;;; Human Player functions
;;;; Used for getting inputs from the human
;;;; ******************************************************************


;;; *********************************************
;;; Name   : print_help
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;;          file_name is the name of the file to
;;;          save the game to
;;; Purpose: Print the optimal move and rationale
;;; Return : None
;;; *********************************************
(defun print_help (game_state)

    ; using let because we will have to call 
    ; get_best_move twice -- once for the move
    ; and the other for the rationale
    (let 
            (
                (move (get_best_move_optimized game_state))
            )

            (format t "Optimal move: ~a~%" move)
            (format t "Rationale: ~a~%" (get_explanation_from_rationales (get_move_rationale game_state move)))
            move
    )

)


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


    (princ "Enter your move, e.g. A10, ask help (h), or quit (q): ")
    (terpri)
    (let
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
                        ; if user wants help, ie. "h"
                        (equal (string-upcase move) "H")
                        (print_help game_state)
                        (get_human_move game_state)
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

    (princ "Both human and computer have the same score")    
    (terpri)
    (princ "Tossing a coin to see who goes first...")
    (terpri)
    (princ "Heads or tails? (h/t): ")
    (terpri)

    (let
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
                (princ "You won the toss! You will be playing the first turn as white.")
                (terpri)
                t
            )

            (
                (equal coin_toss 0)
                (princ "You lost the toss! You will be playing the second turn as black.")
                (terpri)
                nil
            )
        )
    )
)

;;; *********************************************
;;; Name   : human_wants_to_play_again
;;; Args   : None
;;; Purpose: Ask the human if they want to play
;;;          again
;;; Return : t if human wants to play again, nil
;;;          otherwise
;;; *********************************************
(defun human_wants_to_play_again ()
    (princ "Do you want to play again? (y/n): ")
    (terpri)

    (let
        (
            (human_choice (read-line))
        )

        (cond 

            ; if the inputs are invalid, then try again
            (
                (not (member (string-upcase human_choice) '("Y" "N") :test #'string=))
                (princ "Invalid input. Please try again.")
                (terpri)
                (human_wants_to_play_again)
            )

            (
                (equal (string-upcase human_choice) "Y")
                t
            )

            (
                (equal (string-upcase human_choice) "N")
                nil
            )
        )
    )
)


;;;; ******************************************************************
;;;; Name   : human_wants_to_load_game
;;;; Args   : None
;;;; Purpose: Ask the human if they want to load
;;;;          a game
;;;; Return : t if human wants to load a game, nil
;;;;          otherwise
;;;; ******************************************************************
(defun human_wants_to_load_game ()
    (princ "Do you want to load from file? (y/n): ")
    (terpri)

    (let
        (
            (human_choice (read-line))
        )

        (cond 

            ; if the inputs are invalid, then try again
            (
                (not (member (string-upcase human_choice) '("Y" "N") :test #'string=))
                (princ "Invalid input. Please try again.")
                (terpri)
                (human_wants_to_load_game)
            )

            (
                (equal (string-upcase human_choice) "Y")
                t
            )

            (
                (equal (string-upcase human_choice) "N")
                nil
            )
        )
    )
)

;;; *********************************************
;;; Name   : ask_yes_no_question
;;; Args   : question
;;;          question is the question to ask
;;; Purpose: Ask the human a yes or no question
;;;          and validate the input
;;; Return : t if human answers yes, nil otherwise
;;; *********************************************
(defun ask_yes_no_question (question)
    (princ question)
    (terpri)

    (let
        (
            (human_choice (read-line))
        )

        (cond 

            ; if the inputs are invalid, then try again
            (
                (not (member (string-upcase human_choice) '("Y" "N") :test #'string=))
                (princ "Invalid input. Please try again.")
                (terpri)
                (ask_yes_no_question question)
            )

            (
                (equal (string-upcase human_choice) "Y")
                t
            )

            (
                (equal (string-upcase human_choice) "N")
                nil
            )
        )
    )
)


;;; *********************************************
;;; Name   : ask_save_game
;;; Arg    : game_state - the current game state
;;; Purpose: Asks the user if they want to save the game
;;;          and quit
;;; Return : game_state - the current game state
;;;          The game state is returned if the answer is no
;;;          so that the game can continue in the round loop
;;; *********************************************
(defun ask_save_game (game_state)


    (print_game_state game_state)
    (terpri)
    (terpri)

    (let
        (
            (human_wants_to_save (ask_yes_no_question "Do you want to save the game and quit? (y/n): "))
        )

        (cond 
            
            (
                human_wants_to_save
                (save_and_quit game_state)
            )

            (
                (not human_wants_to_save)
                game_state
            )

        )
    )
)



;;;; ******************************************************************
;;;; End of Human Player functions
;;;; ******************************************************************