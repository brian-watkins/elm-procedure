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
            Procedure.get (Helpers.intCommand 27)
              |> Procedure.map (\result -> "Mapped: " ++ String.fromInt result)
              |> Procedure.andThen (\result -> Procedure.get <| Helpers.stringCommand <| result ++ "!!!")
              |> Procedure.run ProcedureTagger TestStringTagger
          )
        |> Helpers.expectValue "Mapped: 27!!!"
  ]


map2Tests : Test
map2Tests =
  describe "when map2 is used"
  [ test "it runs both procedures and transforms the result" <|
    \() ->
      Helpers.procedureCommandTestState
        |> Command.send (\_ ->
            Procedure.map2 (\resultA resultB -> resultA ++ " and " ++ resultB)
                (Procedure.get <| Helpers.stringCommand "First")
                (Procedure.get <| Helpers.stringCommand "Second")
              |> Procedure.run ProcedureTagger TestStringTagger
          )
        |> Helpers.expectValue "First and Second"
  ]