module BreakTests exposing (..)

import Expect
import Test exposing (..)
import TestHelpers as Helpers exposing (Msg(..))
import Procedure


breakTests : Test
breakTests =
  describe "when break is used"
  [ test "it sends the value, and skips any remaining steps" <|
    \() -> 
      Helpers.tryProcedure (\_ -> 
        Procedure.fetch (Helpers.stringCommand "First")
          |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
          |> Procedure.andThen (\result -> Procedure.provide <| result + 28)
          |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
      )
        |> Helpers.expectResult (Err "First, Break!")
  ]

mapErrorTests : Test
mapErrorTests =
  describe "when mapError is used"
  [ describe "when there is an error"
    [ test "it maps the error value" <|
      \() ->
        Helpers.tryProcedure (\_ ->
          Procedure.fetch (Helpers.stringCommand "First")
            |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
            |> Procedure.andThen (\result -> Procedure.provide <| result + 28)
            |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
            |> Procedure.mapError (\error -> "Mapped error: " ++ error)
        )
          |> Helpers.expectResult (Err "Mapped error: First, Break!")
    ]
  , describe "when there is no error"
    [ test "it does nothing" <|
      \() ->
        Helpers.tryProcedure (\_ ->
          Procedure.fetch (Helpers.stringCommand "First")
            |> Procedure.map (\result -> result ++ " mapped!")
            |> Procedure.mapError (\error -> "Mapped error: " ++ error)
        )
          |> Helpers.expectResult (Ok "First mapped!")
    ]
  ]


catchTests : Test
catchTests =
  describe "catch"
  [ describe "when an error occurs"
    [ test "it converts the error to a new procedure" <|
      \() ->
        Helpers.tryProcedure (\_ ->
          Procedure.fetch (Helpers.stringCommand "First")
            |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
            |> Procedure.andThen (\result -> Procedure.provide <| result + 28)
            |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
            |> Procedure.catch (\error -> Procedure.fetch <| Helpers.stringCommand <| "Recovered from error: " ++ error)
        )
          |> Helpers.expectResult (Ok "Recovered from error: First, Break!")
    ]
  , describe "when an error does not occur"
    [ test "it ignores the catch" <|
      \() ->
        Helpers.tryProcedure (\_ ->
          Procedure.fetch (Helpers.stringCommand "First")
            |> Procedure.map String.length
            |> Procedure.andThen (\result -> Procedure.provide <| result + 28)
            |> Procedure.catch (\error -> Procedure.fetch <| Helpers.intCommand 400)
            |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
        )
          |> Helpers.expectResult (Ok "33 mapped!")
    ]
  ]