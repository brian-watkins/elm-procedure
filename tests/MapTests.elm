module MapTests exposing (..)

import Expect
import Test exposing (..)
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


mapTests : Test
mapTests =
  describe "when map is used"
  [ test "it transforms the result and passes it on" <|
    \() -> 
      Helpers.runProcedure (\_ -> 
        Procedure.fetch (Helpers.intCommand 27)
          |> Procedure.map (\result -> "Mapped: " ++ String.fromInt result)
          |> Procedure.andThen (\result -> Procedure.fetch <| Helpers.stringCommand <| result ++ "!!!")
      )
        |> Helpers.expectValue "Mapped: 27!!!"
  ]


map2Tests : Test
map2Tests =
  describe "when map2 is used"
  [ test "it runs both procedures and transforms the result" <|
    \() ->
      Helpers.runProcedure (\_ ->
        Procedure.map2 (\resultA resultB -> resultA ++ " and " ++ resultB)
            (Procedure.fetch <| Helpers.stringCommand "First")
            (Procedure.fetch <| Helpers.stringCommand "Second")
      )
        |> Helpers.expectValue "First and Second"
  ]


map3Tests : Test
map3Tests =
  describe "when map3 is used"
  [ test "it runs all procedures and transforms the result" <|
    \() ->
      Helpers.runProcedure (\_ ->
        Procedure.map3 (\a b c -> a ++ " AND " ++ b ++ " AND " ++ c)
            (Procedure.provide "First")
            (Procedure.provide "Second")
            (Procedure.provide "Third")
      )
        |> Helpers.expectValue "First AND Second AND Third"
  ]
