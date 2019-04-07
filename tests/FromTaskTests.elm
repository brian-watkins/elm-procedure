module FromTaskTests exposing (..)

import Expect
import Test exposing (..)
import Elmer
import Elmer.Command as Command
import TestHelpers as Helpers exposing (Msg(..))
import Procedure
import Task

fromTaskThatCanFailTests : Test
fromTaskThatCanFailTests =
  describe "when fromTask is used with a task that can fail"
  [ describe "when the task fails"
    [ test "it breaks from the procedure" <|
      \() ->
        Helpers.procedureCommandTestState
          |> Command.send (\_ ->
            Procedure.fromTask (
                Task.fail "failed"
                  |> Task.mapError (\e -> "You " ++ e)
              )
              |> Procedure.map (\r -> "Mapped: " ++ r ++ "!")
              |> Procedure.try ProcedureTagger TestResultTagger
          )
          |> Helpers.expectError "You failed"
    ]
  , describe "when the task succeeds"
    [ test "it continues the procedure" <|
      \() ->
        Helpers.procedureCommandTestState
          |> Command.send (\_ ->
            Procedure.fromTask (
                Task.fail "failed"
                  |> Task.mapError (\e -> "You " ++ e)
                  |> Task.onError (\e -> Task.succeed <| "Recovered from: " ++ e)
              )
              |> Procedure.map (\r -> "Mapped: " ++ r ++ "!")
              |> Procedure.try ProcedureTagger TestResultTagger
          )
          |> Helpers.expectValue "Mapped: Recovered from: You failed!"
    ]
  ]

fromTaskThatCannotFailTests : Test
fromTaskThatCannotFailTests =
  describe "when fromTask is used with a task that cannot fail"
  [ test "it runs the task and continues the procedure" <|
    \() ->
      Helpers.procedureCommandTestState
          |> Command.send (\_ ->
            Procedure.fromTask (
                Task.succeed "did it"
                  |> Task.map (\r -> "You " ++ r)
              )
              |> Procedure.map (\r -> "Mapped: " ++ r ++ "!")
              |> Procedure.run ProcedureTagger TestStringTagger
          )
          |> Helpers.expectValue "Mapped: You did it!"
  ]