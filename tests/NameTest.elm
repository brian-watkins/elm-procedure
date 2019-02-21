module NameTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Elmer
import Elmer.Html as Markup
import Elmer.Html.Matchers exposing (element, hasText)
import Elmer.Html.Selector exposing (..)
import Main as App

nameTest : Test
nameTest =
  describe "names"
  [ test "it shows the default name" <|
    \() ->
      Elmer.given App.defaultModel App.view App.update
        |> Markup.target << by [ id "name" ]
        |> Markup.expect (element <| hasText "Your name is: Brian")
  ]