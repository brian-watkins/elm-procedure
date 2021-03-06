module TestApp exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Time exposing (Posix)
import Task
import Http
import Json.Decode as Json
import Json.Encode as Encode
import Procedure
import Procedure.Program


type Msg
  = ProcedureTagger (Procedure.Program.Msg Msg)
  | DoThings
  | ReceivedResponse (Result Http.Error ServerMessage)


type alias ServerMessage =
  { message: String
  }


type alias Model =
  { procedureModel: Procedure.Program.Model Msg
  , serverMessage: String
  }


defaultModel : Model
defaultModel =
  { procedureModel = Procedure.Program.init
  , serverMessage = "Nothing"
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
    ProcedureTagger pMsg ->
      Procedure.Program.update pMsg model.procedureModel
        |> Tuple.mapFirst (\updatedModel -> { model | procedureModel = updatedModel })
    DoThings ->
      ( model
      , Procedure.fetch fetchTime
          |> Procedure.andThen (Procedure.fetch << sendServerRequest)
          |> Procedure.andThen (Procedure.fetch << sendAwesomeRequest)
          |> Procedure.andThen (Procedure.fetch << sendSweetRequest)
          |> Procedure.run ProcedureTagger ReceivedResponse
      )
    ReceivedResponse result ->
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
  Http.post
    { url = "http://funserver.com/api/fun"
    , body = requestBody time
    , expect = Http.expectJson tagger messageDecoder
    }
  -- Http.post "http://funserver.com/api/fun" (requestBody time) messageDecoder
    -- |> Http.send tagger


sendAwesomeRequest : Result Http.Error ServerMessage -> (Result Http.Error ServerMessage -> Msg) -> Cmd Msg
sendAwesomeRequest result tagger =
  case result of
    Ok message ->
      Http.get
        { url = "http://awesomeserver.com/api/awesome?message=" ++ message.message
        , expect = Http.expectJson tagger messageDecoder
        }
      -- Http.get ("http://awesomeserver.com/api/awesome?message=" ++ message.message) messageDecoder
        -- |> Http.send tagger
    Err _ ->
      Cmd.none


sendSweetRequest : Result Http.Error ServerMessage -> (Result Http.Error ServerMessage -> Msg) -> Cmd Msg
sendSweetRequest result tagger =
  case result of
    Ok message ->
      Http.post
        { url = "http://sweetserver.com/api/sweet"
        , body = sweetRequestBody message.message
        , expect = Http.expectJson tagger messageDecoder
        }
      -- Http.post ("http://sweetserver.com/api/sweet") (sweetRequestBody message.message) messageDecoder
        -- |> Http.send tagger
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

