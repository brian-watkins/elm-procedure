module MapTests exposing (..)

import Expect
import Test exposing (..)
import Elmer.Command as Command
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
              |> Procedure.run ProcedureTagger TestStringTagger
          )
        |> Helpers.expectValue "Mapped: 27!!!"
  ]
