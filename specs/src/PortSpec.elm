module PortSpec exposing (main)

import Spec exposing (..)
import Spec.Setup as Setup
import Spec.Markup as Markup
import Spec.Markup.Selector exposing (..)
import Spec.Markup.Event as Event
import Spec.Claim exposing (..)
import Spec.Extra exposing (..)
import Runner
import Json.Encode as Encode
import PortApp as App


subscriptionSpec =
  describe "subscribe to key press"
  [ scenario "key presses occur" (
      given (
        Setup.init (App.init ())
          |> Setup.withUpdate App.update
          |> Setup.withView App.view
          |> Setup.withSubscriptions App.subscriptions
      )
      |> when "keys are pressed"
        [ Markup.target << document
        , keyPressEvent "A"
        , keyPressEvent "B"
        , keyPressEvent "X"
        , keyPressEvent "G"
        ]
      |> it "prints the letter" (
        Markup.observeElement
          |> Markup.query << by [ attributeName "data-on-type" ]
          |> expect (isSomethingWhere <| Markup.text <| equals "You pressed X!!!")
      )
    )
  ]


keyPressEvent char =
  Encode.object
    [ ( "key", Encode.string char )
    ]
    |> Event.trigger "keypress"


main =
  Runner.browserProgram
    [ subscriptionSpec
    ]