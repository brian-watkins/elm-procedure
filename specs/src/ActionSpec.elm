module ActionSpec exposing (main)

import Spec exposing (..)
import Spec.Setup as Setup
import Spec.Markup as Markup
import Spec.Markup.Selector exposing (..)
import Spec.Markup.Event as Event
import Spec.Http exposing (asJson)
import Spec.Http.Stub as Stub exposing (..)
import Spec.Http.Route as Route exposing (route, UrlDescriptor(..))
import Spec.Time as Time
import Spec.Claim exposing (..)
import Spec.Extra exposing (..)
import Runner
import Json.Decode as Json
import Json.Encode as Encode
import TestApp as App


actionSpec =
  describe "procedures"
  [ scenario "running a procedure with tasks and commands" (
      given (
        Setup.initWithModel App.defaultModel
          |> Setup.withView App.view
          |> Setup.withUpdate App.update
          |> Time.withTime 1515281017615
          |> Stub.serve [ serverStub, anotherServerStub, thirdServerStub ]
      )
      |> when "the procedure is initiated"
        [ Markup.target << by [ id "doThingsButton" ]
        , Event.click
        ]
      |> observeThat
        [ it "uses the current time in the first HTTP request" (
            Spec.Http.observeRequests (Route.post "http://funserver.com/api/fun")
              |> expect (isListWhere
                [ Spec.Http.body (asJson <| Json.field "time" Json.int) <| equals 1515281017632
                ]
              )
          )
        , it "passes the server response to another request" (
            Spec.Http.observeRequests (route "GET" <| Matching "http:\\/\\/awesomeserver\\.com\\/api\\/awesome\\?.*")
              |> expect (isListWhere
                [ Spec.Http.url <| isStringContaining 1 "message=some-key"]
              )
          )
        , it "passes the second response to a third request" (
            Spec.Http.observeRequests (Route.post "http://sweetserver.com/api/sweet")
              |> expect (isListWhere
                [ Spec.Http.body (asJson <| Json.field "code" Json.string) <| equals "awesome-key"
                ]
              )
          )
        , it "uses the response message" (
            Markup.observeElement
              |> Markup.query << by [ id "message" ]
              |> expect (isSomethingWhere <| Markup.text <| equals "Cool Server Message!")
          )
        ]
    )
  ]


serverStub =
  Stub.for (Route.post "http://funserver.com/api/fun")
    |> withStatus 201
    |> withBody (withJson <| Encode.object [ ("message", Encode.string "some-key") ])


anotherServerStub =
  Stub.for (route "GET" <| Matching "http:\\/\\/awesomeserver\\.com\\/api\\/awesome\\?.+")
    |> withStatus 200
    |> withBody (withJson <| Encode.object [ ("message", Encode.string "awesome-key") ])


thirdServerStub =
  Stub.for (Route.post "http://sweetserver.com/api/sweet")
    |> withStatus 200
    |> withBody (withJson <| Encode.object [ ("message", Encode.string "Cool Server Message!") ])


main =
  Runner.browserProgram
    [ actionSpec
    ]