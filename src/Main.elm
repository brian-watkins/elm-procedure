module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr


type Msg
  = Msg


type alias Model =
  { name: String
  }


defaultModel : Model
defaultModel =
  { name = "Brian"
  }


view : Model -> Html Msg
view model =
  Html.h1 [ Attr.id "name" ]
  [ Html.text <| "Your name is: " ++ model.name
  ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)


