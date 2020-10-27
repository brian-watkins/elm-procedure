module AcceptFromChannelSpec exposing (main)

import Spec exposing (..)
import Spec.Port as Port
import Helpers exposing (..)
import Runner
import Json.Encode as Encode
import Procedure
import Procedure.Channel as Channel


acceptOneSpec =
  describe "#acceptOne"
  [ scenario "values are received on the channel" (
      runProcedure (
        Channel.join intSubscription
          |> Channel.acceptOne
          |> Procedure.map String.fromInt
      )
      |> when "values are sent on the subscription"
        [ Port.send "intSubscription" <| Encode.int 14
        , Port.send "intSubscription" <| Encode.int 8
        , Port.send "intSubscription" <| Encode.int 27
        ]
      |> expectValues
        [ "14"
        ]
    )
  ]


acceptAllSpec =
  describe "#acceptAll"
  [ scenario "multiple values are received" (
      runProcedure (
        Channel.join intSubscription
          |> Channel.accept
          |> Procedure.map String.fromInt
      )
      |> when "values are sent on the subscription"
        [ Port.send "intSubscription" <| Encode.int 142
        , Port.send "intSubscription" <| Encode.int 81
        , Port.send "intSubscription" <| Encode.int 294
        ]
      |> expectValues
        [ "294"
        , "81"
        , "142"
        ]
    )
  , scenario "multiple values are filtered" (
      runProcedure (
        Channel.join intSubscription
          |> Channel.filter (\_ num -> modBy 2 num == 0)
          |> Channel.accept
          |> Procedure.map String.fromInt
      )
      |> when "values are sent on the subscription"
        [ Port.send "intSubscription" <| Encode.int 142
        , Port.send "intSubscription" <| Encode.int 81
        , Port.send "intSubscription" <| Encode.int 294
        , Port.send "intSubscription" <| Encode.int 13
        , Port.send "intSubscription" <| Encode.int 27
        ]
      |> expectValues
        [ "294"
        , "142"
        ]
    )
  , scenario "waiting on another channel" (
      runProcedure (
        Channel.join intSubscription
          |> Channel.accept
          |> Procedure.map String.fromInt
          |> Procedure.andThen (\result ->
            stringSubscription
              |> Channel.join
              |> Channel.acceptOne
              |> Procedure.map (\word -> "Word for " ++ result ++ ": " ++ word)
          )
          |> Procedure.map (\r -> "After awaiting: " ++ r)
      )
      |> when "values are received on the subscriptions"
        [ Port.send "intSubscription" <| Encode.int 101
        , Port.send "stringSubscription" <| Encode.string "apple"
        , Port.send "stringSubscription" <| Encode.string "cherry"
        , Port.send "intSubscription" <| Encode.int 201
        , Port.send "stringSubscription" <| Encode.string "bike"
        , Port.send "stringSubscription" <| Encode.string "car"
        , Port.send "intSubscription" <| Encode.int 301
        , Port.send "stringSubscription" <| Encode.string "cloud"
        ]
      |> expectValues
        [ "After awaiting: Word for 301: cloud"
        , "After awaiting: Word for 201: bike"
        , "After awaiting: Word for 101: apple"
        ]
    )
  ]


acceptUntilSpec =
  describe "#acceptUntil"
  [ scenario "values are received" (
      runProcedure (
        Channel.join intSubscription
          |> Channel.acceptUntil (\data -> data == 8)
          |> Procedure.map String.fromInt
      )
      |> when "values are received on the subscription"
        [ Port.send "intSubscription" <| Encode.int 22
        , Port.send "intSubscription" <| Encode.int 61
        , Port.send "intSubscription" <| Encode.int 8
        , Port.send "intSubscription" <| Encode.int 92
        , Port.send "intSubscription" <| Encode.int 0
        ]
      |> expectValues
        [ "8"
        , "61"
        , "22"
        ]
    )
  ]


main =
  Runner.browserProgram
    [ acceptOneSpec
    , acceptAllSpec
    , acceptUntilSpec
    ]