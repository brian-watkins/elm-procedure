module ProvideTests exposing (..)

import Expect
import Test exposing (..)
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


provideTests : Test
provideTests =
  describe "when provide is used"
  [ test "it provides the value to the next step" <|
    \() -> 
      Helpers.runProcedure (\_ -> 
        Procedure.fetch (Helpers.stringCommand "First")
          |> Procedure.andThen (\result -> Procedure.provide <| result ++ ", Sent!")
          |> Procedure.andThen (\result -> Procedure.fetch <| Helpers.stringCommand <| result ++ ", Third")
      )
        |> Helpers.expectValue "First, Sent!, Third"
  ]
