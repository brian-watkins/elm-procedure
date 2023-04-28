module SubscribeToChannelSpec exposing (main)

import Spec exposing (..)
import Spec.Port as Port
import Spec.Extra exposing (..)
import Spec.Claim exposing (..)
import Spec.Observer as Observer
import Helpers exposing (..)
import Runner
import Json.Encode as Encode
import Json.Decode as Json
import Procedure
import Procedure.Channel as Channel


subscribeSpec =
  describe "subscribe to channel"
  [ scenario "values are received" (
      runProcedure (
        Channel.join stringSubscription
          |> Channel.acceptOne
          |> Procedure.andThen (\result -> Procedure.fetch <| sendValueCommand <| "After sub: " ++ result)
          |> Procedure.map (\result -> "Mapped: " ++ result)
      )
      |> when "values are received on the subscription"
        [ Port.send "stringSubscription" <| Encode.string "Sub Message"
        , Port.send "stringSubscription" <| Encode.string "Sub Message Two"
        ]
      |> observeThat
        [ expectValues
          [ "Mapped: After sub: Sub Message"
          ]
        , it "removes the subscription" (
            Observer.observeModel identity
              |> expect (\model ->
                equals Sub.none <| testSubscriptions model
              )
          )
        ]
    )
  , scenario "the subscription values are filtered" (
      runProcedure (
        Channel.join objectSubscription
          |> Channel.filter (\_ desc -> desc.key == "sport")
          |> Channel.acceptOne
          |> Procedure.map .value
          |> Procedure.andThen (\result -> Procedure.fetch <| sendValueCommand <| "After sub: " ++ result)
          |> Procedure.map (\result -> "Mapped: " ++ result)
      )
      |> when "values are received on the subscription"
        [ Port.send "objectSubscription" <| Encode.object [ ("key", Encode.string "fruit"), ("value", Encode.string "apple") ]
        , Port.send "objectSubscription" <| Encode.object [ ("key", Encode.string "sport"), ("value", Encode.string "bowling") ]
        , Port.send "objectSubscription" <| Encode.object [ ("key", Encode.string "color"), ("value", Encode.string "red") ]
        ]
      |> expectValue "Mapped: After sub: bowling"
    )
  ]


multipleChannelsSpec =
  let
    runMultipleProcedures =
      runProcedure (
        Channel.join objectSubscription
          |> Channel.filter (\channelId desc -> desc.key == channelId)
          |> Channel.acceptOne
          |> Procedure.map .value
      )
      |> when "another procedure runs"
        [ justRunProcedure (
            Channel.join intSubscription
              |> Channel.filter (\channelId number -> String.contains (String.fromInt number) channelId)
              |> Channel.acceptOne
              |> Procedure.map String.fromInt
          )
        ]
  in
  describe "multiple channels"
  [ scenario "the channelId is filtered" (
      runMultipleProcedures
      |> when "values are received on the subscription"
        [ Port.send "objectSubscription" <| Encode.object [ ("key", Encode.string "Channel-1"), ("value", Encode.string "apple") ]
        ]
      |> expectValue "apple"
    )
  , scenario "multiple procedures filtering on channelId" (
      runMultipleProcedures
      |> when "values are received on the subscription"
        [ Port.send "intSubscription" <| Encode.int 7
        , Port.send "intSubscription" <| Encode.int 7
        , Port.send "intSubscription" <| Encode.int 3
        ]
      |> expectValue "3"
    )
  , scenario "multiple channels are mapped" (
      tryProcedure (
        Procedure.map2 (\a b -> a ++ " AND " ++ b)
          ( Channel.join stringSubscription
              |> Channel.acceptOne
          )
          ( Channel.join intSubscription
              |> Channel.acceptOne
              |> Procedure.map String.fromInt
          )
      )
      |> when "values are received in order different from how the procedures are listed in map2"
        [ Port.send "stringSubscription" <| Encode.string "YO!"
        , Port.send "intSubscription" <| Encode.int 78
        ]
      |> expectResult (Ok "YO! AND 78")
    )
  ]


main =
  Runner.browserProgram
    [ subscribeSpec
    , multipleChannelsSpec
    ]