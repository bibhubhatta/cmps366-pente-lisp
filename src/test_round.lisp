(load "board.lisp")
(load "serials.lisp")
(load "game_state.lisp")
(load "file_handling.lisp")
(load "round.lisp")


(princ "Testing play_round")
(terpri)

(
    conduct_round
        (
            get_initial_state
        )
)