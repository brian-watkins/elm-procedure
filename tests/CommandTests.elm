module CommandTests exposing (..)

import Expect
import Test exposing (..)
import Elmer
import Elmer.Html as Markup
import Elmer.Html.Matchers exposing (element, hasText)
import Elmer.Html.Selector exposing (..)
import Elmer.Html.Event as Event
import Elmer.Http as Http exposing (HttpResponseStub)
import Elmer.Http.Stub as Stub exposing (withBody, withStatus)
import Elmer.Http.Status as Status
import Elmer.Http.Route as Route
import Elmer.Http.Matchers exposing (hasBody)
import Elmer.Spy as Spy
import Main as App
import Task
import Time

timeTest : Test
timeTest =
  describe "time" <|
  let
    state =
      Elmer.given App.defaultModel App.view App.update
        |> Spy.use [ timeSpy 1515281017615, Http.serve [ serverStub ] ]
        |> Markup.target << by [ id "doThingsButton" ]
        |> Event.click
  in  
  [ test "it uses the current time in the request" <|
    \() ->
      state
        |> Http.expect (Route.post "http://funserver.com/api/fun") (
          Elmer.atIndex 0 <| hasBody ("{\"time\":1515281017615}")
        )
  , test "it prints the response message" <|
    \() ->
      state
        |> Markup.target << by [ id "message" ]
        |> Markup.expect (element <| hasText "Cool Server Message!")
  ]


timeSpy time =
  Task.succeed (Time.millisToPosix time)
    |> Spy.replaceValue (\_ -> Time.now)          


serverStub : HttpResponseStub
serverStub =
  Stub.for (Route.post "http://funserver.com/api/fun")
    |> withStatus (httpAcceptedStatus)
    |> withBody "{\"message\":\"Cool Server Message!\"}"


httpAcceptedStatus =
  Status.httpStatus 202 "Accepted"