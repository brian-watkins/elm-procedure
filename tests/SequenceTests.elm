module SequenceTests exposing (..)

import Expect
import Test exposing (..)
import Elmer exposing (TestState)
import Elmer.Command as Command
import Elmer.Spy as Spy exposing (Spy, andCallFake)
import Html exposing (Html)
import Procedure


sequenceTests : Test
sequenceTests =
  describe "when a sequence of commands is executed"
  [ test "it collects the results in a list" <|
    \() -> 
      Elmer.given testModel emptyView testUpdate
        |> Spy.use [ stringCommandSpy, intCommandSpy ]
        |> Command.send (\_ -> 
            Procedure.sequence 
              [ Procedure.first <| fakeStringCommand "Awesome"
              , Procedure.first <| fakeStringCommand "fun"
              , Procedure.first <| fakeStringCommand "stuff!!!"
              ]
              |> Procedure.map (\results -> String.join ", " results)
              |> Procedure.perform CmdTagger TestTagger
          )
        |> expectMappedValue "Awesome, fun, stuff!!!"
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