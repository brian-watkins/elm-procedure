module BreakTests exposing (..)

import Expect
import Test exposing (..)
import Elmer.Command as Command
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


breakTests : Test
breakTests =
  describe "when break is used"
  [ test "it sends the value, and skips any remaining steps" <|
    \() -> 
      Helpers.procedureCommandTestState
        |> Command.send (\_ -> 
            Procedure.get (Helpers.stringCommand "First")
              |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
              |> Procedure.andThen (\result -> Procedure.send <| result + 28)
              |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
              |> Procedure.try ProcedureTagger TestResultTagger
          )
        |> Helpers.expectError "First, Break!"
  ]

mapErrorTests : Test
mapErrorTests =
  describe "when mapError is used"
  [ describe "when there is an error"
    [ test "it maps the error value" <|
      \() ->
        Helpers.procedureCommandTestState
          |> Command.send (\_ ->
              Procedure.get (Helpers.stringCommand "First")
                |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
                |> Procedure.andThen (\result -> Procedure.send <| result + 28)
                |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
                |> Procedure.mapError (\error -> "Mapped error: " ++ error)
                |> Procedure.try ProcedureTagger TestResultTagger
            )
          |> Helpers.expectError "Mapped error: First, Break!"
    ]
  , describe "when there is no error"
    [ test "it does nothing" <|
      \() ->
        Helpers.procedureCommandTestState
          |> Command.send (\_ ->
              Procedure.get (Helpers.stringCommand "First")
                |> Procedure.map (\result -> result ++ " mapped!")
                |> Procedure.mapError (\error -> "Mapped error: " ++ error)
                |> Procedure.try ProcedureTagger TestResultTagger
            )
          |> Helpers.expectValue "First mapped!"
    ]
  ]


catchTests : Test
catchTests =
  describe "catch"
  [ describe "when an error occurs"
    [ test "it converts the error to a new procedure" <|
      \() ->
        Helpers.procedureCommandTestState
          |> Command.send (\_ ->
            Procedure.get (Helpers.stringCommand "First")
              |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
              |> Procedure.andThen (\result -> Procedure.send <| result + 28)
              |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
              |> Procedure.catch (\error -> Procedure.get <| Helpers.stringCommand <| "Recovered from error: " ++ error)
              |> Procedure.try ProcedureTagger TestResultTagger
            )
          |> Helpers.expectValue "Recovered from error: First, Break!"
    ]
  , describe "when an error does not occur"
    [ test "it ignores the catch" <|
      \() ->
        Helpers.procedureCommandTestState
          |> Command.send (\_ ->
            Procedure.get (Helpers.stringCommand "First")
              |> Procedure.map String.length
              |> Procedure.andThen (\result -> Procedure.send <| result + 28)
              |> Procedure.catch (\error -> Procedure.get <| Helpers.intCommand 400)
              |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
              |> Procedure.try ProcedureTagger TestResultTagger
            )
          |> Helpers.expectValue "33 mapped!"
    ]
  ]