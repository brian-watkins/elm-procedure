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
  = CmdHolder (Cmd Msg)
  | DoThings
  | ReceivedAThirdServerResponse (Result Http.Error ServerMessage)


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
    CmdHolder cmd ->
      ( model, cmd )
    DoThings ->
      ( model
      , startAction fetchTime
          |> thenDoAction sendServerRequest
          |> thenDoAction sendAwesomeRequest
          |> thenDoAction sendSweetRequest
          |> performAction ReceivedAThirdServerResponse
      )
    ReceivedAThirdServerResponse result ->
      case result of
        Ok message ->
          ( { model | serverMessage = message.message }, Cmd.none )
        Err _ ->
          ( model, Cmd.none )


fetchTime : (Posix -> Msg) -> Cmd Msg
fetchTime tagger =
  Time.now
    |> Task.perform tagger


sendServerRequest : Posix -> (Result Http.Error ServerMessage -> Msg) -> Cmd Msg
sendServerRequest time tagger =
  Http.post "http://funserver.com/api/fun" (requestBody time) messageDecoder
    |> Http.send tagger


sendAwesomeRequest : Result Http.Error ServerMessage -> (Result Http.Error ServerMessage -> Msg) -> Cmd Msg
sendAwesomeRequest result tagger =
  case result of
    Ok message ->
      Http.get ("http://awesomeserver.com/api/awesome?message=" ++ message.message) messageDecoder
        |> Http.send tagger
    Err _ ->
      Cmd.none


sendSweetRequest : Result Http.Error ServerMessage -> (Result Http.Error ServerMessage -> Msg) -> Cmd Msg
sendSweetRequest result tagger =
  case result of
    Ok message ->
      Http.post ("http://sweetserver.com/api/sweet") (sweetRequestBody message.message) messageDecoder
        |> Http.send tagger
    Err _ ->
      Cmd.none


sweetRequestBody : String -> Http.Body
sweetRequestBody message =
  Encode.object [ ("code", Encode.string message) ]
    |> Http.jsonBody


requestBody : Posix -> Http.Body
requestBody time =
  Encode.object [ ( "time", Encode.int <| Time.posixToMillis time ) ]
    |> Http.jsonBody


messageDecoder : Json.Decoder ServerMessage
messageDecoder =
  Json.field "message" Json.string
    |> Json.map ServerMessage


-------

type alias Action a =
  { generator : (a -> Msg) -> Cmd Msg
  }

startAction : ((a -> Msg) -> Cmd Msg) -> Action a
startAction generator =
  { generator = generator
  }

thenDoAction : (a -> ((b -> Msg) -> Cmd Msg)) -> Action a -> Action b
thenDoAction mapper action =
  { generator = \bTagger -> 
      action.generator <|
        \aData -> 
          bTagger 
            |> mapper aData 
            |> CmdHolder
  }

performAction : (a -> Msg) -> Action a -> Cmd Msg
performAction tagger action =
  action.generator tagger



-- type AppMsg
--   = CmdHolder (Cmd Msg)
--   | ChainedCmdHolder (Cmd AppMsg)
--   | SubMsg Msg


-- type alias AppModel =
--   { subModel : Model
--   }


-- defaultAppModel : AppModel
-- defaultAppModel =
--   { subModel = defaultModel
--   }


-- appView : AppModel -> Html AppMsg
-- appView model =
--   view model.subModel
--     |> Html.map SubMsg


-- appUpdate : AppMsg -> AppModel -> (AppModel, Cmd AppMsg)
-- appUpdate msg model =
--   case msg of
--     CmdHolder cmd ->
--       (model, Cmd.map SubMsg cmd)
--     ChainedCmdHolder cmd ->
--       (model, cmd)
--     SubMsg subMsg ->
--       let
--         ( subModel, subCmd ) = update subMsg model.subModel    
--       in
--         ( { model | subModel = subModel }, subCmd )
