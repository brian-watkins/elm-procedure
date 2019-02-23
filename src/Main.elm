module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Time exposing (Posix)
import Task
import Http
import Json.Decode as Json
import Json.Encode as Encode

type Msg
  = DoThings
  | CurrentTime Posix
  | ReceivedServerResponse (Result Http.Error ServerMessage)


type alias ServerMessage =
  { message: String
  }


type alias Model =
  { serverMessage: String
  }


defaultModel : Model
defaultModel =
  { serverMessage = "Nothing"
  }


view : Model -> Html Msg
view model =
  Html.div []
  [ Html.h1 [ Attr.id "message" ]
    [ Html.text model.serverMessage
    ]
  , Html.button [ Attr.id "doThingsButton", Event.onClick DoThings ] [ Html.text "Click me"]
  ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DoThings ->
      ( model
      , Time.now
          |> Task.perform CurrentTime
      )
    CurrentTime time ->
      ( model
      , Http.post "http://funserver.com/api/fun" (requestBody time) messageDecoder
          |> Http.send ReceivedServerResponse
      )
    ReceivedServerResponse result ->
      case result of
        Ok message ->
          ( { model | serverMessage = message.message }, Cmd.none )
        Err _ ->
          ( model, Cmd.none )


requestBody : Posix -> Http.Body
requestBody time =
  Encode.object [ ( "time", Encode.int <| Time.posixToMillis time ) ]
    |> Http.jsonBody


messageDecoder : Json.Decoder ServerMessage
messageDecoder =
  Json.field "message" Json.string
    |> Json.map ServerMessage