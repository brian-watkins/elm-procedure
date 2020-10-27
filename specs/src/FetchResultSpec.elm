module FetchResultSpec exposing (main)

import Spec exposing (..)
import Helpers exposing (..)
import Runner
import Procedure


fetchResultSpec =
  describe "#fetchResult"
  [ scenario "the result is Ok" (
      tryProcedure (
        Procedure.fetchResult (sendValueCommand <| Ok 0)
          |> Procedure.map (\number -> number + 18)
          |> Procedure.catch (\_ ->
            Procedure.provide 4
          )
      )
      |> expectResult (Ok 18)
    )
  , scenario "the result is not OK" (
      tryProcedure (
        Procedure.fetchResult (sendValueCommand <| Err "Failed!")
      )
      |> expectResult (Err "Failed!")
    )
  , scenario "catching the error" (
      tryProcedure (
        Procedure.fetchResult (sendValueCommand <| Err "Failed!")
            |> Procedure.map (\number -> number + 18)
            |> Procedure.catch (\_ ->
              Procedure.provide 4
            )
      )
      |> expectResult (Ok 4)
    )
  ]


main =
  Runner.browserProgram
    [ fetchResultSpec
    ]