module SendToChannelSpec exposing (main)

import Spec exposing (..)
import Spec.Port as Port
import Spec.Extra exposing (..)
import Spec.Claim exposing (..)
import Helpers exposing (..)
import Runner
import Json.Encode as Encode
import Json.Decode as Json
import Procedure
import Procedure.Channel as Channel


openAndConnectSpec =
  describe "open and connect"
  [ scenario "the channel is opened" (
      runProcedure (
        Channel.open (\_ -> stringPortCommand "Fun!")
          |> Channel.connect stringSubscription
          |> Channel.acceptOne
          |> Procedure.map (\result -> "Mapped: " ++ result)
          |> Procedure.andThen (\result -> Procedure.fetch <| sendValueCommand <| result ++ "!!!")
      )
      |> when "messages are received on the string subscription"
        [ Port.send "stringSubscription" <| Encode.string "window"
        , Port.send "stringSubscription" <| Encode.string "bird"
        ]
      |> observeThat
        [ expectValues [ "Mapped: window!!!" ]
        , it "sends the port command" (
            Port.observe "stringPortCommand" Json.string
              |> expect (isListWhere
                [ equals "Fun!"
                ]
              )
          )
        ]
    )
  , scenario "the channel id is used to open the channel" (
      runProcedure (
        Channel.open (\channelId -> stringPortCommand channelId)
          |> Channel.connect stringSubscription
          |> Channel.acceptOne
      )
      |> when "another procedure runs"
        [ justRunProcedure (
            Channel.open (\channelId -> stringPortCommand channelId)
              |> Channel.connect stringSubscription
              |> Channel.acceptOne
          )
        ]
      |> observeThat
        [ it "sends the commands with distinct channel ids" (
            Port.observe "stringPortCommand" Json.string
              |> expect (isListWhere
                [ equals "0-0"
                , equals "1-0"
                ]
              )
          )
        ]
    )
  , scenario "multiple channels" (
      runProcedure (
        Channel.open (\channelId -> stringPortCommand channelId)
          |> Channel.connect stringSubscription
          |> Channel.filter (\key data -> key == "0-0")
          |> Channel.acceptOne
          |> Procedure.andThen (\_ ->
            Channel.open (\channelId -> stringPortCommand channelId)
              |> Channel.connect stringSubscription
              |> Channel.filter(\key data -> key == "0-1")
              |> Channel.acceptOne
          )
      )
      |> when "values are received"
        [ Port.send "stringSubscription" <| Encode.string "first"
        , Port.send "stringSubscription" <| Encode.string "second"
        , Port.send "stringSubscription" <| Encode.string "third"
        ]
      |> observeThat
        [ it "sends the commands with distinct channel ids" (
            Port.observe "stringPortCommand" Json.string
              |> expect (isListWhere
                [ equals "0-0"
                , equals "0-1"
                ]
              )
          )
        , expectValue "second"
        ]
    )
  ]


main =
  Runner.browserProgram
    [ openAndConnectSpec
    ]