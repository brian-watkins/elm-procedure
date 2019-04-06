module DoTests exposing (..)

import Expect
import Test exposing (..)
import Elmer
import Elmer.Command as Command
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


doTests : Test
doTests =
  describe "when do is used to send a command without a callback" <|
  let
    testState =
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
            Procedure.do (Helpers.stringPortCommand "Hello")
              |> Procedure.run ProcedureTagger TestUnitTagger
        )
  in
  [ test "it sends the command" <|
    \() ->
      testState
        |> Command.expectDummy "string-port"
  , test "it continues by passing a unit to the next step" <|
    \() ->
      testState
        |> Helpers.expectUnit
  ]
