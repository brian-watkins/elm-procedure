module FromTaskTests exposing (..)

import Expect
import Test exposing (..)
import TestHelpers as Helpers exposing (Msg(..))
import Procedure
import Task

fromTaskThatCanFailTests : Test
fromTaskThatCanFailTests =
  describe "when fromTask is used with a task that can fail"
  [ describe "when the task fails"
    [ test "it breaks from the procedure" <|
      \() ->
        Helpers.tryProcedure (\_ ->
          Procedure.fromTask (
              Task.fail "failed"
                |> Task.mapError (\e -> "You " ++ e)
            )
            |> Procedure.map (\r -> "Mapped: " ++ r ++ "!")
        )
          |> Helpers.expectResult (Err "You failed")
    ]
  , describe "when the task succeeds"
    [ test "it continues the procedure" <|
      \() ->
        Helpers.tryProcedure (\_ ->
          Procedure.fromTask (
              Task.fail "failed"
                |> Task.mapError (\e -> "You " ++ e)
                |> Task.onError (\e -> Task.succeed <| "Recovered from: " ++ e)
            )
            |> Procedure.map (\r -> "Mapped: " ++ r ++ "!")
        )
          |> Helpers.expectResult (Ok "Mapped: Recovered from: You failed!")
    ]
  ]

fromTaskThatCannotFailTests : Test
fromTaskThatCannotFailTests =
  describe "when fromTask is used with a task that cannot fail"
  [ test "it runs the task and continues the procedure" <|
    \() ->
      Helpers.runProcedure (\_ ->
        Procedure.fromTask (
            Task.succeed "did it"
              |> Task.map (\r -> "You " ++ r)
          )
          |> Procedure.map (\r -> "Mapped: " ++ r ++ "!")
      )
        |> Helpers.expectValue "Mapped: You did it!"
  ]