module ProvideSpec exposing (main)

import Spec exposing (..)
import Helpers exposing (..)
import Runner
import Procedure


provideSpec =
  describe "#provide"
  [ scenario "using provide" (
      runProcedure (
        Procedure.fetch (sendValueCommand "First")
          |> Procedure.andThen (\result -> Procedure.provide <| result ++ ", Sent!")
          |> Procedure.andThen (\result -> Procedure.fetch <| sendValueCommand <| result ++ ", Third")
      )
      |> expectValue "First, Sent!, Third"
    )
  ]


main =
  Runner.browserProgram
    [ provideSpec
    ]