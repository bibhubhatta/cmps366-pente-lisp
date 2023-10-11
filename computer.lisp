(load "board.lisp")
(load "game_state.lisp")

;;;; ******************************************************************
;;;; Computer Player functions
;;;; Used for getting the computer to play the game
;;;; ******************************************************************



;;; *********************************************
;;; Name   : get_computer_move
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;;          file_name is the name of the file to
;;;          save the game to
;;; Purpose: Get the computer's move for a game
;;;          state
;;; Return : The position -- a string like "A1"
;;; *********************************************
(defun get_computer_move (game_state)

    ; for now, just return a random move
    (nth 
            (random (length (get_available_moves (get_board game_state))))
            
            (get_available_moves (get_board game_state))
    )

)

;;;; ******************************************************************
;;;; End of computer player functions
;;;; ******************************************************************