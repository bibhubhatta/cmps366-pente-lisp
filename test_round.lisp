(load "board.lisp")
(load "serials.lisp")
(load "game_state.lisp")
(load "file_handling.lisp")
(load "round.lisp")


(print "Testing play_round")
(terpri)

(
  print_game_state
        (
          play_round
          (
            case_4
          )
        )
)