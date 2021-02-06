port module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, Attribute)
import Html.Events as Events
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Decoder)
import Procedure
import Procedure.Program
import Procedure.Channel as Channel

port syncPort : String -> Cmd msg
port asyncPort : String -> Cmd msg
port portSubscription : (String -> msg) -> Sub msg

type alias SaveRequest =
  { id: String
  , data: String
  }

type alias SaveResult =
  { id: String
  , data: String
  , success: Bool
  }

port localStorageSaveResult : (SaveResult -> msg) -> Sub msg
port saveToLocalStorage : SaveRequest -> Cmd msg

asyncPortProcedure : String -> Cmd Msg
asyncPortProcedure word =
  Channel.open (\_ -> asyncPort word)
    |> Channel.connect portSubscription
    |> Channel.acceptOne
    |> Procedure.run ProcMsg ReceivedPortMessage


syncPortProcedure : String -> Cmd Msg
syncPortProcedure word =
  Channel.open (\_ -> syncPort word)
    |> Channel.connect portSubscription
    |> Channel.acceptOne
    |> Procedure.run ProcMsg ReceivedPortMessage


keyPressProcedure : Cmd Msg
keyPressProcedure =
  Channel.join (\tagger ->
    Browser.Events.onKeyPress <| Decode.map tagger keyDecoder
  )
    |> Channel.filter (\_ keyPress -> keyPress == "Z" || keyPress == "X" || keyPress == "Y")
    |> Channel.acceptUntil (\_ -> False)
    |> Procedure.map (\key -> key ++ "!!!")
    |> Procedure.run ProcMsg PressedKey


keyDecoder : Decoder String
keyDecoder =
  Decode.field "key" Decode.string


saveWordProcedure : String -> Cmd Msg
saveWordProcedure word =
  Channel.open (\channelKey -> saveToLocalStorage { id = channelKey, data = word } )
    |> Channel.connect localStorageSaveResult
    |> Channel.filter (\channelKey result -> result.id == channelKey)
    |> Channel.acceptOne
    |> Procedure.map .data
    |> Procedure.run ProcMsg SavedWord


saveNumberProcedure : Int -> Cmd Msg
saveNumberProcedure number =
  Channel.open (\channelKey -> saveToLocalStorage { id = channelKey, data = String.fromInt number } )
    |> Channel.connect localStorageSaveResult
    |> Channel.filter (\channelKey result -> result.id == channelKey)
    |> Channel.acceptOne
    |> Procedure.map (\saveResult -> String.toInt saveResult.data |> Maybe.withDefault -1)
    |> Procedure.run ProcMsg SavedNumber


---


type Msg
  = ProcMsg (Procedure.Program.Msg Msg)
  | ReceivedPortMessage String
  | PressedKey String
  | HandleInput String
  | TriggerAsyncPort
  | TriggerSyncPort
  | TriggerSaveWord
  | TriggerSaveNumber
  | SavedWord String
  | SavedNumber Int

type alias Model =
  { portMessage: String
  , didPressSpecialKey: Maybe String
  , portInput: String
  , procModel: (Procedure.Program.Model Msg)
  , saveResults: List String
  }

defaultModel : Model
defaultModel =
  { portMessage = ""
  , didPressSpecialKey = Nothing
  , portInput = ""
  , procModel = Procedure.Program.init
  , saveResults = []
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( defaultModel, keyPressProcedure )

view : Model -> Html Msg
view model =
  Html.div []
  [ Html.h1 [ dataAttribute "data-on-type" ] [ pressedKeyMessage model ]
  , Html.div []
    [ Html.input [ dataAttribute "data-port-input", Attr.value model.portInput, Events.onInput HandleInput ] []
    , Html.button [ dataAttribute "data-port-async-submit", Events.onClick TriggerAsyncPort ] [ Html.text "Click Me for Async" ]
    , Html.button [ dataAttribute "data-port-sync-submit", Events.onClick TriggerSyncPort ] [ Html.text "Click Me for Synchronous" ]
    , Html.button [ dataAttribute "data-word-save", Events.onClick TriggerSaveWord ] [ Html.text "Save a Word" ]
    , Html.button [ dataAttribute "data-number-save", Events.onClick TriggerSaveNumber ] [ Html.text "Save a Number" ]
    ]
  , Html.div [ dataAttribute "data-port-message" ]
    [ Html.text model.portMessage 
    ]
  , Html.pre [ dataAttribute "data-save-messages" ]
    [ Html.text <| String.join "\n" model.saveResults
    ]
  ]

dataAttribute : String -> Attribute Msg
dataAttribute name =
  Attr.attribute name ""

pressedKeyMessage : Model -> Html Msg
pressedKeyMessage model =
  case model.didPressSpecialKey of
    Just key ->
      Html.text <| "You pressed " ++ key
    Nothing ->
      Html.text "You have not yet pressed X, Y, or Z."

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ProcMsg pMsg ->
      Procedure.Program.update pMsg model.procModel
        |> Tuple.mapFirst (\updated -> { model | procModel = updated })
    ReceivedPortMessage message ->
      ( { model | portMessage = message }, Cmd.none )
    PressedKey key ->
      ( { model | didPressSpecialKey = Just key }, Cmd.none )
    HandleInput input ->
      ( { model | portInput = input }, Cmd.none )
    TriggerAsyncPort ->
      ( model, asyncPortProcedure model.portInput )
    TriggerSyncPort ->
      ( model, syncPortProcedure model.portInput )
    TriggerSaveWord ->
      ( { model | portInput = "" }, saveWordProcedure model.portInput )
    TriggerSaveNumber ->
      ( { model | portInput = "" }
      , String.toInt model.portInput
          |> Maybe.withDefault -1
          |> saveNumberProcedure
      )
    SavedWord word ->
      ( { model | saveResults = List.append model.saveResults [ "You saved a word: " ++ word ] }, Cmd.none )
    SavedNumber number ->
      ( { model | saveResults = List.append model.saveResults [ "You saved a number: " ++ String.fromInt number ] }, Cmd.none )
    
subscriptions : Model -> Sub Msg
subscriptions model =
  Procedure.Program.subscriptions model.procModel

---

main =
  Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }