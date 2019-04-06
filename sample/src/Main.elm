port module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, Attribute)
import Html.Events as Events
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import Procedure
import Procedure.Channel as Channel

port syncPort : String -> Cmd msg
port asyncPort : String -> Cmd msg
port portSubscription : (String -> msg) -> Sub msg

asyncPortProcedure : String -> Cmd Msg
asyncPortProcedure word =
  Channel.send (\_ -> asyncPort word)
    |> Channel.receive portSubscription
    |> Procedure.await
    |> Procedure.run ProcMsg ReceivedPortMessage

syncPortProcedure : String -> Cmd Msg
syncPortProcedure word =
  Channel.send (\_ -> syncPort word)
    |> Channel.receive portSubscription
    |> Procedure.await
    |> Procedure.run ProcMsg ReceivedPortMessage

keyPressProcedure : Cmd Msg
keyPressProcedure =
  Channel.subscribe (\tagger ->
    Browser.Events.onKeyPress <| Decode.map tagger keyDecoder
  )
    |> Channel.filter (\_ keyPress -> keyPress == "Z")
    |> Procedure.await
    |> Procedure.map (\_ -> True)
    |> Procedure.run ProcMsg PressedZ

keyDecoder : Decoder String
keyDecoder =
  Decode.field "key" Decode.string

---

type Msg
  = ProcMsg (Procedure.Msg Msg)
  | ReceivedPortMessage String
  | PressedZ Bool
  | HandleInput String
  | TriggerAsyncPort
  | TriggerSyncPort

type alias Model =
  { portMessage: String
  , didPressZ: Bool
  , portInput: String
  , procModel: (Procedure.Model Msg)
  }

defaultModel : Model
defaultModel =
  { portMessage = ""
  , didPressZ = False
  , portInput = ""
  , procModel = Procedure.init
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( defaultModel, keyPressProcedure )

view : Model -> Html Msg
view model =
  Html.div []
  [ Html.h1 [ dataAttribute "data-on-type" ] [ pressedKeyMessage model ]
  , Html.div []
    [ Html.input [ dataAttribute "data-port-input", Events.onInput HandleInput ] []
    , Html.button [ dataAttribute "data-port-async-submit", Events.onClick TriggerAsyncPort ] [ Html.text "Click Me for Async" ]
    , Html.button [ dataAttribute "data-port-sync-submit", Events.onClick TriggerSyncPort ] [ Html.text "Click Me for Synchronous" ]
    ]
  , Html.div [ dataAttribute "data-port-message" ]
    [ Html.text model.portMessage 
    ]
  ]

dataAttribute : String -> Attribute Msg
dataAttribute name =
  Attr.attribute name ""

pressedKeyMessage : Model -> Html Msg
pressedKeyMessage model =
  if model.didPressZ then
    Html.text "You pressed Z!!!"
  else
    Html.text "You have not yet pressed Z."

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ProcMsg pMsg ->
      Procedure.update pMsg model.procModel
        |> Tuple.mapFirst (\updated -> { model | procModel = updated })
    ReceivedPortMessage message ->
      ( { model | portMessage = message }, Cmd.none )
    PressedZ didPress ->
      ( { model | didPressZ = didPress }, Cmd.none )
    HandleInput input ->
      ( { model | portInput = input }, Cmd.none )
    TriggerAsyncPort ->
      ( model, asyncPortProcedure model.portInput )
    TriggerSyncPort ->
      ( model, syncPortProcedure model.portInput )
    
subscriptions : Model -> Sub Msg
subscriptions model =
  Procedure.subscriptions model.procModel

---

main =
  Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }