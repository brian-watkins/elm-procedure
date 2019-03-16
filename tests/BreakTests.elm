module BreakTests exposing (..)

import Expect
import Test exposing (..)
import Elmer exposing (TestState)
import Elmer.Command as Command
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Html exposing (Html)
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


breakTests : Test
breakTests =
  describe "when break is used"
  [ test "it sends the value, and skips any remaining steps" <|
    \() -> 
      Helpers.procedureCommandTestState
        |> Command.send (\_ -> 
            Procedure.do (Helpers.stringCommand "First")
              |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
              |> Procedure.andThen (\result -> Procedure.send <| result + 28)
              |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
              |> Procedure.try CmdTagger TestResultTagger
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
              Procedure.do (Helpers.stringCommand "First")
                |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
                |> Procedure.andThen (\result -> Procedure.send <| result + 28)
                |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
                |> Procedure.mapError (\error -> "Mapped error: " ++ error)
                |> Procedure.try CmdTagger TestResultTagger
            )
          |> Helpers.expectError "Mapped error: First, Break!"
    ]
  , describe "when there is no error"
    [ test "it does nothing" <|
      \() ->
        Helpers.procedureCommandTestState
          |> Command.send (\_ ->
              Procedure.do (Helpers.stringCommand "First")
                |> Procedure.map (\result -> result ++ " mapped!")
                |> Procedure.mapError (\error -> "Mapped error: " ++ error)
                |> Procedure.try CmdTagger TestResultTagger
            )
          |> Helpers.expectValue "First mapped!"
    ]
  ]
