module BreakSpec exposing (main)

import Spec exposing (..)
import Helpers exposing (..)
import Runner
import Procedure


breakSpec =
  describe "#break"
  [ scenario "breaking from a procedure" (
      tryProcedure (
        Procedure.fetch (sendValueCommand "First")
          |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
          |> Procedure.andThen (\result -> Procedure.provide <| result + 28)
          |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
      )
      |> expectResult (Err "First, Break!")
    )
  , scenario "recovering after break" (
      tryProcedure (
        Procedure.fetch (sendValueCommand "First")
          |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
          |> Procedure.andThen (\result -> Procedure.provide <| result + 28)
          |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
          |> Procedure.catch (\error -> Procedure.fetch <| sendValueCommand <| "Recovered from error: " ++ error)
      )
      |> expectResult (Ok "Recovered from error: First, Break!")
    )
  , scenario "no recovery since no break" (
      tryProcedure (
        Procedure.fetch (sendValueCommand "First")
          |> Procedure.map String.length
          |> Procedure.andThen (\result -> Procedure.provide <| result + 28)
          |> Procedure.catch (\error -> Procedure.fetch <| sendValueCommand 400)
          |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
      )
      |> expectResult (Ok "33 mapped!")
    )
  ]


mapErrorSpec =
  describe "#mapError"
  [ scenario "there is an error to map" (
      tryProcedure (
        Procedure.fetch (sendValueCommand "First")
          |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
          |> Procedure.andThen (\result -> Procedure.provide <| result + 28)
          |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
          |> Procedure.mapError (\error -> "Mapped error: " ++ error)
      )
      |> expectResult (Err "Mapped error: First, Break!")
    )
  , scenario "there is no error to map" (
      tryProcedure (
        Procedure.fetch (sendValueCommand "First")
          |> Procedure.map (\result -> result ++ " mapped!")
          |> Procedure.mapError (\error -> "Mapped error: " ++ error)
      )
      |> expectResult (Ok "First mapped!")
    )
  ]


main =
  Runner.browserProgram
    [ breakSpec
    , mapErrorSpec
    ]