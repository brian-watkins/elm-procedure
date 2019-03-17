module SendTests exposing (..)

import Expect
import Test exposing (..)
import Elmer.Command as Command
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


sendTests : Test
sendTests =
  describe "when send is used"
  [ test "it sends the value" <|
    \() -> 
      Helpers.procedureCommandTestState
        |> Command.send (\_ -> 
            Procedure.get (Helpers.stringCommand "First")
              |> Procedure.andThen (\result -> Procedure.send <| result ++ ", Sent!")
              |> Procedure.andThen (\result -> Procedure.get <| Helpers.stringCommand <| result ++ ", Third")
              |> Procedure.run ProcedureTagger TestStringTagger
          )
        |> Helpers.expectValue "First, Sent!, Third"
  ]
