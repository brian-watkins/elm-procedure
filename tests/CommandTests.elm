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
import Elmer.Http.Matchers exposing (hasBody, hasQueryParam)
import Elmer.Spy as Spy
import Main as App
import Task
import Time

timeTest : Test
timeTest =
  describe "time" <|
  let
    state =
      Elmer.given App.defaultAppModel App.appView App.appUpdate
        |> Spy.use [ timeSpy 1515281017615, Http.serve [ serverStub, anotherServerStub, thirdServerStub ] ]
        |> Markup.target << by [ id "doThingsButton" ]
        |> Event.click
  in  
  [ test "it uses the current time in the request" <|
    \() ->
      state
        |> Http.expect (Route.post "http://funserver.com/api/fun") (
          Elmer.atIndex 0 <| hasBody ("{\"time\":1515281017615}")
        )
  , test "it passes the server response to another request" <|
    \() ->
      state
        |> Http.expect (Route.get "http://awesomeserver.com/api/awesome") (
          Elmer.atIndex 0 <| hasQueryParam ("message", "some-key")
        )
  , test "it passes the second response to a third request" <|
    \() ->
      state
        |> Http.expect (Route.post "http://sweetserver.com/api/sweet") (
          Elmer.atIndex 0 <| hasBody ("{\"code\":\"awesome-key\"}")
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
    |> withBody "{\"message\":\"some-key\"}"


anotherServerStub : HttpResponseStub
anotherServerStub =
  Stub.for (Route.get "http://awesomeserver.com/api/awesome")
    |> withStatus Status.ok
    |> withBody "{\"message\":\"awesome-key\"}"


thirdServerStub : HttpResponseStub
thirdServerStub =
  Stub.for (Route.post "http://sweetserver.com/api/sweet")
    |> withStatus Status.ok
    |> withBody "{\"message\":\"Cool Server Message!\"}"


httpAcceptedStatus =
  Status.httpStatus 202 "Accepted"