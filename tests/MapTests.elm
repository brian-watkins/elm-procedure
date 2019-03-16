module MapTests exposing (..)

import Expect
import Test exposing (..)
import Elmer exposing (TestState)
import Elmer.Command as Command
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Html exposing (Html)
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


mapTests : Test
mapTests =
  describe "when map is used"
  [ test "it transforms the result and passes it on" <|
    \() -> 
      Helpers.procedureCommandTestState
        |> Command.send (\_ -> 
            Procedure.do (Helpers.intCommand 27)
              |> Procedure.map (\result -> "Mapped: " ++ String.fromInt result)
              |> Procedure.andThen (\result -> Procedure.do <| Helpers.stringCommand <| result ++ "!!!")
              |> Procedure.run CmdTagger TestStringTagger
          )
        |> Helpers.expectValue "Mapped: 27!!!"
  ]
