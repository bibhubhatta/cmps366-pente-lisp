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

;;; *********************************************
;;; Name   : load_game_state_from_file
;;; Args   : file_name
;;;          file_name is the name of the file to
;;;          load the game from
;;; Purpose: Load the game from a file
;;; Return : game_state
;;; *********************************************
(defun load_game_state_from_file (file_name)
    (with-open-file 
    
      (stream file_name :direction :input :if-does-not-exist :error)
    
      (read stream)
    
    )
)

;;; *********************************************
;;; Name   : load_game_from_user_location
;;; Args   : none
;;; Purpose: Load the game from a file
;;; Return : game_state
;;; *********************************************
(defun load_game_state_from_user_location ()

  (princ "Enter the name of the file you want to load the game from: ")
  ; without terpri, the input prompt
  ; is not displayed until the user enters a name
  (terpri)

  (let*  
      
      ((file_name (read-line)) )

      (handler-case

          
          (print_game_state (load_game_state_from_file file_name))

          (error (condition)
            (princ "Error loading game state from file: ")
            (princ file_name)
            (terpri)
            (princ "Error message: ")
            (princ condition)
            (terpri)

            (princ "Try again")
            (terpri)
            (load_game_state_from_user_location)
          )

          (:no-error (condition)
            (princ "Game state loaded from file: ")
            (princ file_name)
            (terpri)
            
            ; because we are printing the game state
            ; to check if it is correct, we need to
            ; load it again
           (load_game_state_from_file file_name)
          )
      )

  )

)

;;;; ******************************************************************
;;;; End of File Handling Functions
;;;; ******************************************************************