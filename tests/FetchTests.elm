module FetchTests exposing (..)

import Expect
import Test exposing (..)
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


fetchResultTests : Test
fetchResultTests =
  describe "when fetchResult is used"
  [ describe "when the result is Ok" 
    [ test "it passes the value on" <|
      \() ->
        Helpers.tryProcedure (\_ ->
          Procedure.fetchResult (Helpers.resultCommand <| Ok 0)
          |> Procedure.map (\number -> number + 18)
          |> Procedure.catch (\_ ->
            Procedure.provide 4
          )
        )
        |> Helpers.expectResult (Ok 18)
    ]
  , describe "when the result is not ok"
    [ test "it passes the error on" <|
      \() ->
        Helpers.tryProcedure (\_ ->
          Procedure.fetchResult (Helpers.resultCommand <| Err "Failed!")
        )
        |> Helpers.expectResult (Err "Failed!")
    , test "it breaks from the procedure" <|
      \() ->
        Helpers.tryProcedure (\_ ->
          Procedure.fetchResult (Helpers.resultCommand <| Err "Failed!")
            |> Procedure.map (\number -> number + 18)
            |> Procedure.catch (\_ ->
              Procedure.provide 4
            )
        )
        |> Helpers.expectResult (Ok 4)
    ]
  ]
