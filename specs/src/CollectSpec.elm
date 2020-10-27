module CollectSpec exposing (main)

import Spec exposing (..)
import Helpers exposing (..)
import Runner
import Procedure


collectSpec =
  describe "#collect"
  [ scenario "no error" (
      runProcedure (
        Procedure.collect
          [ Procedure.fetch <| sendValueCommand "Awesome"
          , Procedure.fetch <| sendValueCommand "fun"
          , Procedure.fetch <| sendValueCommand "stuff!!!"
          ]
          |> Procedure.map (\results -> String.join ", " results)
      )
      |> expectValue "Awesome, fun, stuff!!!"
    )
  , scenario "interrupted by an error" (
      tryProcedure (
        Procedure.collect
          [ Procedure.fetch <| sendValueCommand "Awesome"
          , Procedure.break "Break!?"
          , Procedure.fetch <| sendValueCommand "stuff!!!"
          , Procedure.fetch <| sendValueCommand "more stuff!!!"
          ]
          |> Procedure.map (\results -> String.join ", " results)
      )
      |> expectResult (Err "Break!?")
    )
  ]


main =
  Runner.browserProgram
    [ collectSpec
    ]