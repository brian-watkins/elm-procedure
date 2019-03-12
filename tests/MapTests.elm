module MapTests exposing (..)

import Expect
import Test exposing (..)
import Elmer exposing (TestState)
import Elmer.Command as Command
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Html exposing (Html)
import Procedure


mapTests : Test
mapTests =
  describe "when map is used"
  [ test "it transforms the result and passes it on" <|
    \() -> 
      Elmer.given testModel emptyView testUpdate
        |> Spy.use [ stringCommandSpy, intCommandSpy ]
        |> Command.send (\_ -> 
            Procedure.first (fakeIntCommand 27)
              |> Procedure.map (\result -> "Mapped: " ++ String.fromInt result)
              |> Procedure.andThen (\result -> Procedure.first <| fakeStringCommand <| result ++ "!!!")
              |> Procedure.perform CmdTagger TestTagger
          )
        |> expectMappedValue "Mapped: 27!!!"
  ]

expectMappedValue : String -> TestState Model Msg -> Expect.Expectation
expectMappedValue expected testState =
  testState
    |> Elmer.expectModel (\model ->
        Expect.equal model.message expected
    )

stringCommandSpy : Spy
stringCommandSpy =
  Spy.observe (\_ -> fakeStringCommand)
    |> andCallFake (\word tagger -> 
      tagger word |> Command.fake
    )


intCommandSpy : Spy
intCommandSpy =
  Spy.observe (\_ -> fakeIntCommand)
    |> andCallFake (\number tagger -> 
      tagger number |> Command.fake
    )


fakeStringCommand : String -> (String -> Msg) -> Cmd Msg
fakeStringCommand _ _ =
  Cmd.none


fakeIntCommand : Int -> (Int -> Msg) -> Cmd Msg
fakeIntCommand _ _ =
  Cmd.none


type Msg
  = CmdTagger (Cmd Msg)
  | TestTagger String

type alias Model =
  { message : String
  }

testModel =
  { message = ""
  }

testUpdate : Msg -> Model -> (Model, Cmd Msg)
testUpdate msg model =
  case msg of
    CmdTagger cmd ->
      (model, cmd)
    TestTagger value ->
      ({ model | message = value }, Cmd.none)


emptyView : Model -> Html Msg
emptyView _ =
  Html.text ""