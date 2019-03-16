module SendTests exposing (..)

import Expect
import Test exposing (..)
import Elmer exposing (TestState)
import Elmer.Command as Command
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Html exposing (Html)
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


sendTests : Test
sendTests =
  describe "when send is used"
  [ test "it sends the value" <|
    \() -> 
      Helpers.procedureCommandTestState
        |> Command.send (\_ -> 
            Procedure.do (Helpers.stringCommand "First")
              |> Procedure.andThen (\result -> Procedure.send <| result ++ ", Sent!")
              |> Procedure.andThen (\result -> Procedure.do <| Helpers.stringCommand <| result ++ ", Third")
              |> Procedure.run CmdTagger TestStringTagger
          )
        |> Helpers.expectValue "First, Sent!, Third"
  ]
