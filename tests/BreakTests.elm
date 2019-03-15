module BreakTests exposing (..)

import Expect
import Test exposing (..)
import Elmer exposing (TestState)
import Elmer.Command as Command
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Html exposing (Html)
import Procedure


breakTests : Test
breakTests =
  describe "when break is used"
  [ test "it sends the value, and skips any remaining steps" <|
    \() -> 
      Elmer.given testModel emptyView testUpdate
        |> Spy.use [ stringCommandSpy ]
        |> Command.send (\_ -> 
            Procedure.do (fakeStringCommand "First")
              |> Procedure.andThen (\result -> Procedure.break <| result ++ ", Break!")
              |> Procedure.andThen (\result -> Procedure.send <| result + 28)
              |> Procedure.map (\result -> String.fromInt result ++ " mapped!")
              |> Procedure.try CmdTagger TestTagger
          )
        |> expectError "First, Break!"
  ]

expectError : String -> TestState Model Msg -> Expect.Expectation
expectError expected testState =
  testState
    |> Elmer.expectModel (\model ->
        Expect.equal model.error expected
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
  | TestTagger (Result String String)

type alias Model =
  { message : String
  , error : String
  }

testModel =
  { message = ""
  , error = ""
  }

testUpdate : Msg -> Model -> (Model, Cmd Msg)
testUpdate msg model =
  case msg of
    CmdTagger cmd ->
      (model, cmd)
    TestTagger value ->
      case value of
        Ok data ->
          ({ model | message = data }, Cmd.none)
        Err data ->
          ({ model | error = data }, Cmd.none)

emptyView : Model -> Html Msg
emptyView _ =
  Html.text ""