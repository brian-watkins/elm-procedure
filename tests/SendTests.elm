module SendTests exposing (..)

import Expect
import Test exposing (..)
import Elmer exposing (TestState)
import Elmer.Command as Command
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Html exposing (Html)
import Procedure


sendTests : Test
sendTests =
  describe "when send is used"
  [ test "it sends the value" <|
    \() -> 
      Elmer.given testModel emptyView testUpdate
        |> Spy.use [ stringCommandSpy ]
        |> Command.send (\_ -> 
            Procedure.do (fakeStringCommand "First")
              |> Procedure.andThen (\result -> Procedure.send <| result ++ ", Sent!")
              |> Procedure.andThen (\result -> Procedure.do <| fakeStringCommand <| result ++ ", Third")
              |> Procedure.perform CmdTagger TestTagger
          )
        |> expectMappedValue "First, Sent!, Third"
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

fakeStringCommand : String -> (String -> Msg) -> Cmd Msg
fakeStringCommand _ _ =
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