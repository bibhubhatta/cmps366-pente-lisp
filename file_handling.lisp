;;;; ******************************************************************
;;;; File Handling Functions
;;;; Used for reading from and writing to files
;;;; ******************************************************************



;;; *********************************************
;;; Name   : save_game_to_file
;;; Args   : game_state, file_name
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;;          file_name is the name of the file to
;;;          save the game to
;;; Purpose: Save the game to a file
;;; Return : nil
;;; *********************************************
(defun save_game_to_file (game_state file_name)
    (with-open-file 
    
      (stream file_name :direction :output :if-exists :supersede :if-does-not-exist :create)
    
      (print game_state stream)
    
    )
)

;;; *********************************************
;;; Name   : save_game_to_user_location
;;; Args   : game_state
;;;          game_state is the game state like
;;;          the one in the serialization lists
;;; Purpose: Save the game to a file
;;; Return : nil
;;; *********************************************
(defun save_game_to_user_location (game_state)


  (princ "Enter the name of the file you want to save the game to: ")
  ; without terpri, the input prompt
  ; is not displayed until the user enters a name
  (terpri)

  (let*  
      
      ((file_name (read-line)) )

      (handler-case

          
          (save_game_to_file game_state file_name)

          (error (condition)
            (princ "Error saving game state to file: ")
            (princ file_name)
            (terpri)
            (princ "Error message: ")
            (princ condition)
            (terpri)

            (princ "Try again")
            (terpri)
            (save_game_to_user_location game_state)
          )

          (:no-error (condition)
            (princ "Game state saved to file: ")
            (princ file_name)
            (terpri)
          )
      )

  ) 
)

;;;; ******************************************************************
;;;; End of File Handling Functions
;;;; ******************************************************************