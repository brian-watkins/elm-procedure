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
  | ReceivedServerResponse (Result Http.Error ServerMessage)
  | ReceivedAnotherServerResponse (Result Http.Error ServerMessage)


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


update : Msg -> Model -> (Model, Cmd AppMsg)
update msg model =
  case msg of
    DoThings ->
      ( model
      , Time.now
          |> Task.perform (superTagger)
      )
    ReceivedServerResponse result ->
      case result of
        Ok message ->
          ( model, Cmd.map SubMsg <| sendAnotherServerRequest message.message )
        Err _ ->
          ( model, Cmd.none )
    ReceivedAnotherServerResponse result ->
      case result of
        Ok message ->
          ( { model | serverMessage = message.message }, Cmd.none )
        Err _ ->
          ( model, Cmd.none )


superTagger : Posix -> AppMsg
superTagger time =
  sendServerRequest time
    |> CmdHolder


sendServerRequest time =
  Http.post "http://funserver.com/api/fun" (requestBody time) messageDecoder
    |> Http.send ReceivedServerResponse


sendAnotherServerRequest message =
  Http.get ("http://awesomeserver.com/api/awesome?message=" ++ message) messageDecoder
    |> Http.send ReceivedAnotherServerResponse


requestBody : Posix -> Http.Body
requestBody time =
  Encode.object [ ( "time", Encode.int <| Time.posixToMillis time ) ]
    |> Http.jsonBody


messageDecoder : Json.Decoder ServerMessage
messageDecoder =
  Json.field "message" Json.string
    |> Json.map ServerMessage


-------

thenDo : (a -> Cmd Msg) -> a -> Cmd AppMsg
thenDo tagger item =
  tagger item
    |> Task.succeed
    |> Task.perform CmdHolder


-------


type AppMsg
  = CmdHolder (Cmd Msg)
  | SubMsg Msg


type alias AppModel =
  { subModel : Model
  }


defaultAppModel : AppModel
defaultAppModel =
  { subModel = defaultModel
  }


appView : AppModel -> Html AppMsg
appView model =
  view model.subModel
    |> Html.map SubMsg


appUpdate : AppMsg -> AppModel -> (AppModel, Cmd AppMsg)
appUpdate msg model =
  case msg of
    CmdHolder cmd ->
      (model, Cmd.map SubMsg cmd)
    SubMsg subMsg ->
      let
        ( subModel, subCmd ) = update subMsg model.subModel    
      in
        ( { model | subModel = subModel }, subCmd )
