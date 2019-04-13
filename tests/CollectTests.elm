module CollectTests exposing (..)

import Expect
import Test exposing (..)
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


collectTests : Test
collectTests =
  describe "when a list of steps is executed"
  [ test "it collects the results in a list" <|
    \() ->
      Helpers.runProcedure (\_ ->
        Procedure.collect
          [ Procedure.fetch <| Helpers.stringCommand "Awesome"
          , Procedure.fetch <| Helpers.stringCommand "fun"
          , Procedure.fetch <| Helpers.stringCommand "stuff!!!"
          ]
          |> Procedure.map (\results -> String.join ", " results)
      )
        |> Helpers.expectValue "Awesome, fun, stuff!!!"
  ]

breakTests : Test
breakTests =
  describe "when a seqeunce of commands is interrupted"
  [ test "it returns the error only" <|
    \() ->
      Helpers.tryProcedure (\_ ->
        Procedure.collect
          [ Procedure.fetch <| Helpers.stringCommand "Awesome"
          , Procedure.break "Break!?"
          , Procedure.fetch <| Helpers.stringCommand "stuff!!!"
          , Procedure.fetch <| Helpers.stringCommand "more stuff!!!"
          ]
          |> Procedure.map (\results -> String.join ", " results)
      )
        |> Helpers.expectResult (Err "Break!?")
  ]
