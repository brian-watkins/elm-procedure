module FromTaskSpec exposing (main)

import Spec exposing (..)
import Helpers exposing (..)
import Runner
import Task
import Procedure


fromTaskSpec =
  describe "#fromTask"
  [ scenario "the task fails" (
      tryProcedure (
        Procedure.fromTask (
          Task.fail "failed"
            |> Task.mapError (\e -> "You " ++ e)
        )
          |> Procedure.map (\r -> "Mapped: " ++ r ++ "!")
      )
      |> expectResult (Err "You failed")
    )
  , scenario "the task succeeds" (
      tryProcedure (
        Procedure.fromTask (
          Task.fail "failed"
            |> Task.mapError (\e -> "You " ++ e)
            |> Task.onError (\e -> Task.succeed <| "Recovered from: " ++ e)
        )
          |> Procedure.map (\r -> "Mapped: " ++ r ++ "!")
      )
      |> expectResult (Ok "Mapped: Recovered from: You failed!")
    )
  , scenario "task that always succeeds" (
      runProcedure (
        Procedure.fromTask (
            Task.succeed "did it"
              |> Task.map (\r -> "You " ++ r)
          )
          |> Procedure.map (\r -> "Mapped: " ++ r ++ "!")
      )
      |> expectValue "Mapped: You did it!"
    )
  ]


main =
  Runner.browserProgram
    [ fromTaskSpec
    ]