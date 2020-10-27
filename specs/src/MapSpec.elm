module MapSpec exposing (main)

import Spec exposing (..)
import Helpers exposing (..)
import Runner
import Procedure


mapSpec =
  describe "mapping"
  [ scenario "#map" (
      runProcedure (
        Procedure.fetch (sendValueCommand 27)
          |> Procedure.map (\result -> "Mapped: " ++ String.fromInt result)
          |> Procedure.andThen (\result -> Procedure.fetch <| sendValueCommand <| result ++ "!!!")
      )
      |> expectValue "Mapped: 27!!!"
    )
  , scenario "#map2" (
      runProcedure (
        Procedure.map2 (\resultA resultB -> resultA ++ " and " ++ resultB)
            (Procedure.fetch <| sendValueCommand "First")
            (Procedure.fetch <| sendValueCommand "Second")
      )
      |> expectValue "First and Second"
    )
  , scenario "#map3" (
      runProcedure (
        Procedure.map3 (\a b c -> a ++ " AND " ++ b ++ " AND " ++ c)
            (Procedure.provide "First")
            (Procedure.provide "Second")
            (Procedure.provide "Third")
      )
      |> expectValue "First AND Second AND Third"
    )
  ]


main =
  Runner.browserProgram
    [ mapSpec
    ]